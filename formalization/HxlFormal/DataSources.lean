namespace HxlFormal

universe u

structure DataSources where
  Source : Type u
  Key : Source -> Type u
  Value : Source -> Type u

structure Atom (ds : DataSources.{u}) where
  source : ds.Source
  key : ds.Key source

end HxlFormal
