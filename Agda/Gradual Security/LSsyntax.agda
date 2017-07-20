module LSsyntax where

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

_~⋎~_ : (ℓ₁ ℓ₂ : Label) → Label
⊤ ~⋎~ ✭ = ⊤
✭ ~⋎~ ⊤ = ⊤
ℓ₁ ~⋎~ ✭ = ℓ₁
✭ ~⋎~ ℓ₂ = ℓ₂
⊤ ~⋎~ ⊤ = ⊤
⊤ ~⋎~ ⊥ = ⊤
⊥ ~⋎~ ⊤ = ⊤
⊥ ~⋎~ ⊥ = ⊥

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
    ℓ≤✭ : ∀ ℓ → ℓ ≤ ✭

_≤?_ : (ℓ₁ ℓ₂ : Label) → Dec (ℓ₁ ≤ ℓ₂) 
⊤ ≤? ⊤ = yes ⊤≤⊤
⊤ ≤? ⊥ = no (λ ())
⊤ ≤? ✭ = yes (ℓ≤✭ ⊤)
⊥ ≤? ⊤ = yes ⊥≤⊤
⊥ ≤? ⊥ = yes ⊥≤⊥
⊥ ≤? ✭ = yes (ℓ≤✭ ⊥)
✭ ≤? ⊤ = no (λ ())
✭ ≤? ⊥ = no (λ ())
✭ ≤? ✭ = yes (ℓ≤✭ ✭)

data _≾_ : GType → GType → Set where
    yes : (t₁ t₂ : GType) → t₁ ≾ t₂
    no  : (t₁ t₂ : GType) → t₁ ≾ t₂

_≾?_ : (t₁ t₂ : GType) → t₁ ≾ t₂
bool x ≾? bool x₁ with (x ≤? x₁)
bool x ≾? bool x₁ | yes p = yes (bool x) (bool x₁)
bool x ≾? bool x₁ | no ¬p = no (bool x) (bool x₁)
bool x ≾? t₂ = no (bool x) t₂
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ with (x ≤? x₁)
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ | yes p with t₃ ≾? t₁ | t₂ ≾? t₄
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ | yes p | (yes .t₃ .t₁) | (yes .t₂ .t₄) = yes ((t₁ ⇒ x) t₂) ((t₃ ⇒ x₁) t₄)
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ | yes p | (yes .t₃ .t₁) | (no .t₂ .t₄) = no ((t₁ ⇒ x) t₂) ((t₃ ⇒ x₁) t₄)
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ | yes p | (no .t₃ .t₁) | (yes .t₂ .t₄) = no ((t₁ ⇒ x) t₂) ((t₃ ⇒ x₁) t₄)
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ | yes p | (no .t₃ .t₁) | (no .t₂ .t₄) = no ((t₁ ⇒ x) t₂) ((t₃ ⇒ x₁) t₄)
(t₁ ⇒ x) t₂ ≾? (t₃ ⇒ x₁) t₄ | no ¬p = no ((t₁ ⇒ x) t₂) ((t₃ ⇒ x₁) t₄)
(t₁ ⇒ x) t₂ ≾? t₃ = no ((t₁ ⇒ x) t₂) t₃
t₁ ≾? t₂ = no t₁ t₂
