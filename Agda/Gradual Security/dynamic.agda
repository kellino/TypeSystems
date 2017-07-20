module dynamic where

open import LSsyntax
open import Relation.Nullary
open import Data.Product

data _⊓ˡ_ : (ℓ₁ ℓ₂ : Label) → Set where
    ℓ⊓✭ : ∀ ℓ → ℓ ⊓ˡ ✭
    ✭⊓ℓ : ∀ ℓ → ✭ ⊓ˡ ℓ
    idℓ : ∀ ℓ → ℓ ⊓ˡ ℓ

_⊓ᵣ_ : ∀ (ℓ₁ ℓ₂ : Label) → Dec (ℓ₁ ⊓ˡ ℓ₂)
⊤ ⊓ᵣ ⊤ = yes (idℓ ⊤)
⊤ ⊓ᵣ ⊥ = no (λ ())
⊤ ⊓ᵣ ✭ = yes (ℓ⊓✭ ⊤)
⊥ ⊓ᵣ ⊤ = no (λ ())
⊥ ⊓ᵣ ⊥ = yes (idℓ ⊥)
⊥ ⊓ᵣ ✭ = yes (ℓ⊓✭ ⊥)
✭ ⊓ᵣ ⊤ = yes (✭⊓ℓ ⊤)
✭ ⊓ᵣ ⊥ = yes (✭⊓ℓ ⊥)
✭ ⊓ᵣ ✭ = yes (ℓ⊓✭ ✭)

-- note that this is not actually correct. The meet of ⊤ and bottom for example should be undefined,
-- but that isn't easy to model directly in Agda. However, if we only call this after induction over 
-- the proof of _⊓ᵣ_ it (should) be impossible to call one of the incorrect cases.
recMeet : ∀ (ℓ₁ ℓ₂ : Label) → Label
recMeet ⊤ ⊤ = ⊤
recMeet ⊤ ✭ = ⊤
recMeet ⊥ ⊥ = ⊥
recMeet ⊥ ✭ = ⊥
recMeet ✭ ⊤ = ⊤
recMeet ✭ ⊥ = ⊥
recMeet ✭ ✭ = ✭
recMeet _ _ = ✭     -- fake case

_⊓_ : ∀ (t₁ t₂ : GType) → GType
bool x ⊓ bool x₁ with x ⊓ᵣ x₁
bool x ⊓ bool x₁ | yes p = bool (recMeet x x₁)
bool x ⊓ bool x₁ | no ¬p = err
bool x ⊓ _ = err
(t₁ ⇒ x) t₂ ⊓ (t₃ ⇒ x₁) t₄ with x ⊓ᵣ x₁
(t₁ ⇒ x) t₂ ⊓ (t₃ ⇒ x₁) t₄ | yes p with (t₁ ⊓ t₃) | (t₂ ⊓ t₄)
(t₁ ⇒ x₂) t₂ ⊓ (t₃ ⇒ x₃) t₄ | yes p | (bool x) | (bool x₁) = ((bool x) ⇒ (recMeet x₂ x₃)) (bool x)
-- this is probably not correct, but it keeps things simple. 
(t₁ ⇒ x₂) t₂ ⊓ (t₃ ⇒ x₃) t₄ | yes p | (bool x) | ((d ⇒ x₁) d₁) = err
(t₁ ⇒ x₂) t₂ ⊓ (t₃ ⇒ x₃) t₄ | yes p | ((c ⇒ x) c₁) | (bool x₁) = err
(t₁ ⇒ x₂) t₂ ⊓ (t₃ ⇒ x₃) t₄ | yes p | ((c ⇒ x) c₁) | ((d ⇒ x₁) d₁) = err
(t₁ ⇒ x₁) t₂ ⊓ (t₃ ⇒ x₂) t₄ | yes p | (bool x) | err = err
(t₁ ⇒ x₁) t₂ ⊓ (t₃ ⇒ x₂) t₄ | yes p | ((c ⇒ x) c₁) | err = err
(t₁ ⇒ x₁) t₂ ⊓ (t₃ ⇒ x₂) t₄ | yes p | err | (bool x) = err
(t₁ ⇒ x₁) t₂ ⊓ (t₃ ⇒ x₂) t₄ | yes p | err | ((d ⇒ x) d₁) = err
(t₁ ⇒ x) t₂ ⊓ (t₃ ⇒ x₁) t₄ | yes p | err | err = err
(t₁ ⇒ x) t₂ ⊓ (t₃ ⇒ x₁) t₄ | no ¬p = err
_ ⊓ _ = err


-- consistent transitivity
_∘<_ : ∀ (ev₁ : GType × GType) (ev₂ : GType × GType) → (GType × GType × GType)
(π₁ , π₂) ∘< (π′₁ , π′₂) = π₁ , (π₂ ⊓ π′₁) , π′₂

