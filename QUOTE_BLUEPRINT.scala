/*
 * Blueprint only. This is not intended to compile.
 *
 * Goal: sketch a realistic quote workflow where Hxl keeps per-quote values in
 * lexical scope, while manual batching must shuttle values through maps.
 */

package hxl.blueprint

import cats.implicits._
import hxl.Hxl

object QuoteBlueprint {
  final case class QuoteReq(
      quoteId: QuoteId,
      customerId: CustomerId,
      cartId: CartId,
      shipTo: AddressId,
      couponCode: Option[CouponCode]
  )

  final case class Quote(
      id: QuoteId,
      customer: CustomerSummary,
      lines: List[QuotedLine],
      discounts: List[Discount],
      shipping: ShippingQuote,
      tax: TaxQuote,
      total: Money
  )

  final case class QuoteLineInput(line: CartLine, customer: Customer, account: Account, market: Market)
  final case class PricedLine(line: CartLine, product: Product, inventory: Inventory, price: Price) {
    def subtotal: Money =
      Money(price.amount.cents * line.quantity)
  }
  final case class CouponInput(customer: Customer, account: Account, lines: List[PricedLine], code: CouponCode)
  final case class ShippingInput(address: Address, account: Account, lines: List[PricedLine])
  final case class TaxInput(address: Address, customer: Customer, lines: List[PricedLine], shipping: ShippingQuote)
  final case class ValidQuoteReq(value: QuoteReq)

  final case class QuoteId(value: String)
  final case class CustomerId(value: String)
  final case class CartId(value: String)
  final case class AddressId(value: String)
  final case class CouponCode(value: String)
  final case class Sku(value: String)
  final case class Market(value: String)
  final case class Money(cents: Long)

  final case class Customer(id: CustomerId, accountId: AccountId, segment: Segment, market: Market)
  final case class CustomerSummary(id: CustomerId, segment: Segment)
  final case class AccountId(value: String)
  final case class Account(id: AccountId, tier: Tier, paymentTerms: PaymentTerms)
  final case class Cart(id: CartId, lines: List[CartLine])
  final case class ValidCart(value: Cart) {
    def lines: List[CartLine] =
      value.lines
  }
  final case class CartLine(lineId: String, sku: Sku, quantity: Int)
  final case class Address(id: AddressId, country: String, postalCode: String)
  final case class Product(sku: Sku, taxableCategory: String, hazmat: Boolean)
  final case class Inventory(sku: Sku, available: Int, backorderable: Boolean)
  final case class Price(amount: Money)
  final case class Discount(code: String, amount: Money)
  final case class ShippingQuote(amount: Money, carrier: String)
  final case class TaxQuote(amount: Money)
  final case class QuotedLine(lineId: String, sku: Sku, quantity: Int, unitPrice: Money)
  final case class Segment(value: String)
  final case class Tier(value: String)
  final case class PaymentTerms(value: String)

  /*
   * Data-source-facing functions. Each one is batchable across many quotes.
   * Implementations intentionally omitted.
   */
  def getCustomer[F[_]](id: CustomerId): Hxl[F, Customer] = ???
  def getAccount[F[_]](id: AccountId): Hxl[F, Account] = ???
  def getCart[F[_]](id: CartId): Hxl[F, Cart] = ???
  def getAddress[F[_]](id: AddressId): Hxl[F, Address] = ???
  def getProduct[F[_]](sku: Sku): Hxl[F, Product] = ???
  def getInventory[F[_]](sku: Sku): Hxl[F, Inventory] = ???
  def getPrice[F[_]](input: QuoteLineInput): Hxl[F, Price] = ???
  def getCouponDiscount[F[_]](input: CouponInput): Hxl[F, Discount] = ???
  def quoteShipping[F[_]](input: ShippingInput): Hxl[F, ShippingQuote] = ???
  def quoteTax[F[_]](input: TaxInput): Hxl[F, TaxQuote] = ???

  /*
   * Pure business rules stay pure. Hxl is only for data dependencies that can
   * batch across many quote programs.
   */
  def validateQuoteReq(req: QuoteReq): Either[String, ValidQuoteReq] = ???
  def validateCart(cart: Cart): Either[String, ValidCart] = ???
  def expectValidCart(cart: Cart): ValidCart = ???
  def canApplyCoupon(customer: Customer, account: Account, code: CouponCode): Boolean = ???

  def getQuote[F[_]](valid: ValidQuoteReq): Hxl[F, Quote] = {
    val req = valid.value

    (for {
      customer <- getCustomer(req.customerId).monadic
      account <- getAccount(customer.accountId).monadic
      cart <- getCart(req.cartId).map(expectValidCart).monadic
      address <- getAddress(req.shipTo).monadic
      lines <- cart.lines.traverse(line => priceLine(line, customer, account)).monadic
      discounts <- couponDiscounts(req, customer, account, lines).monadic
      shipping <- quoteShipping(ShippingInput(address, account, lines)).monadic
      tax <- quoteTax(TaxInput(address, customer, lines, shipping)).monadic
    } yield assembleQuote(req, customer, lines, discounts, shipping, tax)).hxl
  }

  def priceLine[F[_]](line: CartLine, customer: Customer, account: Account): Hxl[F, PricedLine] =
    (for {
      product <- getProduct(line.sku).monadic
      inventory <- getInventory(line.sku).monadic
      price <- getPrice(QuoteLineInput(line, customer, account, customer.market)).monadic
    } yield PricedLine(line, product, inventory, price)).hxl

  def couponDiscounts[F[_]](req: QuoteReq, customer: Customer, account: Account, lines: List[PricedLine]): Hxl[F, List[Discount]] =
    req.couponCode match {
      case Some(code) if canApplyCoupon(customer, account, code) =>
        getCouponDiscount(CouponInput(customer, account, lines, code)).map(List(_))
      case _ =>
        Hxl.pure[F, List[Discount]](Nil)
    }

  def assembleQuote(
      req: QuoteReq,
      customer: Customer,
      lines: List[PricedLine],
      discounts: List[Discount],
      shipping: ShippingQuote,
      tax: TaxQuote
  ): Quote =
    Quote(
      id = req.quoteId,
      customer = CustomerSummary(customer.id, customer.segment),
      lines = lines.map(line => QuotedLine(line.line.lineId, line.line.sku, line.line.quantity, line.price.amount)),
      discounts = discounts,
      shipping = shipping,
      tax = tax,
      total = Money(
        lines.map(_.subtotal.cents).sum +
          shipping.amount.cents +
          tax.amount.cents -
          discounts.map(_.amount.cents).sum
      )
    )

  /*
   * Manual batching sketch for the same business shape.
   *
   * This is the map plumbing Hxl is meant to avoid. Each derived stage must
   * recover original per-quote context by looking through previous result maps.
   */
  def getQuotesManually[F[_]](validRequests: List[ValidQuoteReq]): F[List[Quote]] = {
    val requests = validRequests.map(_.value)

    for {
      customers <- getCustomers(requests.map(_.customerId))
      accounts <- getAccounts(requests.map(req => customers(req.customerId).accountId))
      carts <- getCarts(requests.map(_.cartId))
      validCarts = carts.view.mapValues(expectValidCart).toMap
      addresses <- getAddresses(requests.map(_.shipTo))

      lineRefs = requests.flatMap(req => validCarts(req.cartId).lines.map(req -> _))
      products <- getProducts(lineRefs.map { case (_, line) => line.sku })
      inventory <- getInventories(lineRefs.map { case (_, line) => line.sku })

      priceInputs = lineRefs.map { case (req, line) =>
        val customer = customers(req.customerId)
        QuoteLineInput(line, customer, accounts(customer.accountId), customer.market)
      }
      prices <- getPrices(priceInputs)

      pricedLinesByQuote = requests.map { req =>
        req -> validCarts(req.cartId).lines.map { line =>
          val customer = customers(req.customerId)
          val input = QuoteLineInput(line, customer, accounts(customer.accountId), customer.market)
          PricedLine(line, products(line.sku), inventory(line.sku), prices(input))
        }
      }.toMap

      couponInputs = requests.flatMap { req =>
        req.couponCode.map { code =>
          val customer = customers(req.customerId)
          req -> CouponInput(customer, accounts(customer.accountId), pricedLinesByQuote(req), code)
        }.filter { case (_, input) => canApplyCoupon(input.customer, input.account, input.code) }
      }
      couponDiscounts <- getCouponDiscounts(couponInputs.map(_._2))
      discountsByQuote = requests.map { req =>
        req -> couponInputs.collect { case (`req`, input) => couponDiscounts(input) }
      }.toMap

      shippingInputs = requests.map { req =>
        val customer = customers(req.customerId)
        ShippingInput(addresses(req.shipTo), accounts(customer.accountId), pricedLinesByQuote(req))
      }
      shipping <- getShippingQuotes(shippingInputs)

      taxInputs = requests.map { req =>
        val customer = customers(req.customerId)
        val ship = ShippingInput(addresses(req.shipTo), accounts(customer.accountId), pricedLinesByQuote(req))
        TaxInput(addresses(req.shipTo), customer, pricedLinesByQuote(req), shipping(ship))
      }
      tax <- getTaxQuotes(taxInputs)
    } yield requests.map { req =>
      val customer = customers(req.customerId)
      val ship = ShippingInput(addresses(req.shipTo), accounts(customer.accountId), pricedLinesByQuote(req))
      val tax0 = TaxInput(addresses(req.shipTo), customer, pricedLinesByQuote(req), shipping(ship))
      assembleQuote(req, customer, pricedLinesByQuote(req), discountsByQuote(req), shipping(ship), tax(tax0))
    }
  }

  /*
   * Ordered-list batching sketch for the same business shape.
   *
   * This avoids map lookups by relying on every batch function returning values
   * in input order. The cost is that every stage must carry enough row context
   * to line results back up with the original quote.
   */
  def getQuotesWithLists[F[_]](validRequests: List[ValidQuoteReq]): F[List[Quote]] = {
    val requests = validRequests.map(_.value)

    for {
      customers <- getCustomersList(requests.map(_.customerId))
      accounts <- getAccountsList(customers.map(_.accountId))
      carts <- getCartsList(requests.map(_.cartId))
      validCarts = carts.map(expectValidCart)
      addresses <- getAddressesList(requests.map(_.shipTo))

      quoteRows = requests.zip(customers).zip(accounts).zip(validCarts).zip(addresses).map {
        case ((((req, customer), account), cart), address) =>
          (req, customer, account, cart, address)
      }

      lineRows = quoteRows.flatMap { case (req, customer, account, cart, address) =>
        cart.lines.map(line => (req, customer, account, address, line))
      }
      products <- getProductsList(lineRows.map { case (_, _, _, _, line) => line.sku })
      inventory <- getInventoriesList(lineRows.map { case (_, _, _, _, line) => line.sku })

      lineRowsWithCatalog = lineRows.zip(products).zip(inventory).map {
        case (((req, customer, account, address, line), product), stock) =>
          (req, customer, account, address, line, product, stock)
      }

      priceInputs = lineRowsWithCatalog.map { case (_, customer, account, _, line, _, _) =>
        QuoteLineInput(line, customer, account, customer.market)
      }
      prices <- getPricesList(priceInputs)

      pricedLines = lineRowsWithCatalog.zip(prices).map {
        case ((_, _, _, _, line, product, stock), price) =>
          PricedLine(line, product, stock, price)
      }

      quoteRowsWithLines = quoteRows
        .foldLeft((pricedLines, List.empty[(QuoteReq, Customer, Account, Address, List[PricedLine])])) {
          case ((remaining, rows), (req, customer, account, cart, address)) =>
            val (lines, rest) = remaining.splitAt(cart.lines.length)
            (rest, (req, customer, account, address, lines) :: rows)
        }
        ._2
        .reverse

      couponSlots = quoteRowsWithLines.map { case (req, customer, account, _, lines) =>
        req.couponCode
          .map(code => CouponInput(customer, account, lines, code))
          .filter(input => canApplyCoupon(input.customer, input.account, input.code))
      }
      couponInputs = couponSlots.flatten
      couponDiscounts <- getCouponDiscountsList(couponInputs)
      quoteRowsWithDiscounts = quoteRowsWithLines
        .zip(couponSlots)
        .foldLeft((couponDiscounts, List.empty[(QuoteReq, Customer, Account, Address, List[PricedLine], List[Discount])])) {
          case ((discount :: rest, rows), ((req, customer, account, address, lines), Some(_))) =>
            (rest, (req, customer, account, address, lines, List(discount)) :: rows)
          case ((remaining, rows), ((req, customer, account, address, lines), None)) =>
            (remaining, (req, customer, account, address, lines, Nil) :: rows)
          case ((Nil, _), ((_, _, _, _, _), Some(_))) =>
            sys.error("missing coupon discount")
        }
        ._2
        .reverse

      shippingInputs = quoteRowsWithDiscounts.map { case (_, _, account, address, lines, _) =>
        ShippingInput(address, account, lines)
      }
      shipping <- getShippingQuotesList(shippingInputs)
      quoteRowsWithShipping = quoteRowsWithDiscounts.zip(shipping).map {
        case ((req, customer, account, address, lines, discounts), ship) =>
          (req, customer, account, address, lines, discounts, ship)
      }

      taxInputs = quoteRowsWithShipping.map { case (_, customer, _, address, lines, _, ship) =>
        TaxInput(address, customer, lines, ship)
      }
      tax <- getTaxQuotesList(taxInputs)
    } yield quoteRowsWithShipping.zip(tax).map {
      case ((req, customer, _, _, lines, discounts, shipping), tax0) =>
        assembleQuote(req, customer, lines, discounts, shipping, tax0)
    }
  }

  def getCustomers[F[_]](ids: List[CustomerId]): F[Map[CustomerId, Customer]] = ???
  def getAccounts[F[_]](ids: List[AccountId]): F[Map[AccountId, Account]] = ???
  def getCarts[F[_]](ids: List[CartId]): F[Map[CartId, Cart]] = ???
  def getAddresses[F[_]](ids: List[AddressId]): F[Map[AddressId, Address]] = ???
  def getProducts[F[_]](skus: List[Sku]): F[Map[Sku, Product]] = ???
  def getInventories[F[_]](skus: List[Sku]): F[Map[Sku, Inventory]] = ???
  def getPrices[F[_]](inputs: List[QuoteLineInput]): F[Map[QuoteLineInput, Price]] = ???
  def getCouponDiscounts[F[_]](inputs: List[CouponInput]): F[Map[CouponInput, Discount]] = ???
  def getShippingQuotes[F[_]](inputs: List[ShippingInput]): F[Map[ShippingInput, ShippingQuote]] = ???
  def getTaxQuotes[F[_]](inputs: List[TaxInput]): F[Map[TaxInput, TaxQuote]] = ???

  def getCustomersList[F[_]](ids: List[CustomerId]): F[List[Customer]] = ???
  def getAccountsList[F[_]](ids: List[AccountId]): F[List[Account]] = ???
  def getCartsList[F[_]](ids: List[CartId]): F[List[Cart]] = ???
  def getAddressesList[F[_]](ids: List[AddressId]): F[List[Address]] = ???
  def getProductsList[F[_]](skus: List[Sku]): F[List[Product]] = ???
  def getInventoriesList[F[_]](skus: List[Sku]): F[List[Inventory]] = ???
  def getPricesList[F[_]](inputs: List[QuoteLineInput]): F[List[Price]] = ???
  def getCouponDiscountsList[F[_]](inputs: List[CouponInput]): F[List[Discount]] = ???
  def getShippingQuotesList[F[_]](inputs: List[ShippingInput]): F[List[ShippingQuote]] = ???
  def getTaxQuotesList[F[_]](inputs: List[TaxInput]): F[List[TaxQuote]] = ???
}
