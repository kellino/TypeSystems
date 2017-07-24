module lambdasyntax where

open import Data.Nat using (ℕ)
open import Data.Bool using (Bool; true; false)
open import Data.Vec using (Vec; []; lookup)
open import Relation.Nullary
open import Data.Empty hiding (⊥)
open import Relation.Binary.PropositionalEquality

data Label : Set where
    ⊤ ⊥ ✭ : Label

data GType : Set where
    bool : Label → GType
    _⇒_  : GType → Label → GType → GType
    err  : GType

infixr 4 _∙_
infixl 6 _∨_ _∧_
data Term : Set where
    var           : ℕ → Term
    litBool       : Bool → Label → Term
    lam           : GType → Term → Label → Term
    _∧_           : Term → Term → Term
    _∨_           : Term → Term → Term
    _∙_           : Term → Term → Term
    if_then_else_ : Term → Term → Term → Term
    error         : Term

Ctx : ℕ → Set
Ctx = Vec GType

getLabel : GType → Label
getLabel (bool ℓ) = ℓ
getLabel ((g ⇒ ℓ) g₁) = ℓ
getLabel err = ✭ -- lazy propagation

setLabel : GType → Label → GType
setLabel (bool _) ℓ = bool ℓ
setLabel ((t ⇒ x) t₁) ℓ = (t ⇒ ℓ) t₁
setLabel err _ = err

-- gradual join
_~⋎~_ : (ℓ₁ ℓ₂ : Label) → Label
⊤ ~⋎~ ⊤ = ⊤
⊤ ~⋎~ ⊥ = ⊤
⊤ ~⋎~ ✭ = ⊤
⊥ ~⋎~ ⊤ = ⊤
⊥ ~⋎~ ⊥ = ⊥
⊥ ~⋎~ ✭ = ✭
✭ ~⋎~ ⊤ = ⊤
✭ ~⋎~ ⊥ = ✭
✭ ~⋎~ ✭ = ✭

-- gradual meet
_~⋏~_ : ∀ (ℓ₁ ℓ₂ : Label) → Label
⊥ ~⋏~ ✭ = ⊥
✭ ~⋏~ ⊥ = ⊥
ℓ₁ ~⋏~ ✭ = ✭
✭ ~⋏~ ℓ₂ = ✭
⊤ ~⋏~ ⊤ = ⊤
⊤ ~⋏~ ⊥ = ⊥
⊥ ~⋏~ ⊤ = ⊥
⊥ ~⋏~ ⊥ = ⊥

_:∧:_ : ∀ (t₁ t₂ : GType) → GType
_:∨:_ : ∀ (t₁ t₂ : GType) → GType
bool ℓ₁ :∧: bool ℓ₂ = bool (ℓ₁ ~⋏~ ℓ₂)
(s₁₁ ⇒ ℓ₁) s₁₂ :∧: (s₂₁ ⇒ ℓ₂) s₂₂ = ((s₁₁ :∨: s₂₁) ⇒ (ℓ₁ ~⋏~ ℓ₂)) (s₁₂ :∧: s₂₂)
_ :∧: _ = err
-- _:∨:_ : ∀ (t₁ t₂ : gtype) → gtype
bool ℓ₁ :∨: bool ℓ₂ = bool (ℓ₁ ~⋎~ ℓ₂)
(s₁₁ ⇒ ℓ₁) s₁₂ :∨: (s₂₁ ⇒ ℓ₂) s₂₂ = ((s₁₁ :∧: s₂₁) ⇒ (ℓ₁ ~⋎~ ℓ₂)) (s₁₂ :∨: s₂₂)
_ :∨: _ = err

data _≤_ : Label → Label → Set where
    ⊥≤⊤ : ⊥ ≤ ⊤
    ⊤≤⊤ : ⊤ ≤ ⊤
    ⊥≤⊥ : ⊥ ≤ ⊥
    ℓ≤✭ : ∀ {ℓ} → ℓ ≤ ✭
    ✭≤ℓ : ∀ {ℓ} → ✭ ≤ ℓ

_≤?_ : (ℓ₁ ℓ₂ : Label) → Dec (ℓ₁ ≤ ℓ₂) 
⊤ ≤? ⊤ = yes ⊤≤⊤
⊤ ≤? ⊥ = no (λ ())
⊤ ≤? ✭ = yes (ℓ≤✭)
⊥ ≤? ⊤ = yes ⊥≤⊤
⊥ ≤? ⊥ = yes ⊥≤⊥
⊥ ≤? ✭ = yes (ℓ≤✭)
✭ ≤? ⊤ = yes (✭≤ℓ)
✭ ≤? ⊥ = yes (✭≤ℓ)
✭ ≤? ✭ = yes (ℓ≤✭)

data _≾_ : GType → GType → Set where
    yes : (t₁ t₂ : GType) → t₁ ≾ t₂
    no  : (t₁ t₂ : GType) → t₁ ≾ t₂

_≾?_ : (t₁ t₂ : GType) → t₁ ≾ t₂
bool ℓ ≾? bool ℓ₁ with ℓ ≤? ℓ₁ 
...      | yes p = yes (bool ℓ) (bool ℓ₁)
...      | no ¬p = no (bool ℓ) (bool ℓ₁)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ with t₃ ≾? t₁ | t₂ ≾? t₄ | ℓ ≤? ℓ₁
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | yes .t₃ .t₁ | (yes .t₂ .t₄) | (yes p) = yes ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)

(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | yes .t₃ .t₁ | (yes .t₂ .t₄) | (no ¬p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | yes .t₃ .t₁ | (no .t₂ .t₄) | (yes p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | yes .t₃ .t₁ | (no .t₂ .t₄) | (no ¬p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | no .t₃ .t₁ | (yes .t₂ .t₄) | (yes p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | no .t₃ .t₁ | (yes .t₂ .t₄) | (no ¬p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | no .t₃ .t₁ | (no .t₂ .t₄) | (yes p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)
(t₁ ⇒ ℓ) t₂ ≾? (t₃ ⇒ ℓ₁) t₄ | no .t₃ .t₁ | (no .t₂ .t₄) | (no ¬p) = no ((t₁ ⇒ ℓ) t₂) ((t₃ ⇒ ℓ₁) t₄)

-- everything else is no
t₁ ≾? t₂ = no t₁ t₂
