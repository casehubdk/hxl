/*
 * Copyright 2023 CaseHubDK
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package hxl

/*
 * A unique key for a data source.
 * Every data source type should be uniquely identified by a key,
 * such that Hxl can group the batched keys by their DSkey.
 */
trait DSKey[K, V]

object DSKey {
  def apply[K, V]: DSKey[K, V] = new DSKey[K, V] {}
}
