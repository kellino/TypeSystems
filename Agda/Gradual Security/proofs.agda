module proofs where

open import lambdasyntax
open import static
open import dynamic

open import Data.Empty
open import Data.Nat
open import Function
open import Data.Vec using (Vec; [])
open import Data.Bool using (Bool; true; false)
open import Relation.Nullary
open import Relation.Binary.PropositionalEquality

-- label precision
data _⊑ˡ_ : (ℓ₁ ℓ₂ : Label) → Set where
    ℓ⊑✭ : ∀ {ℓ} → ℓ ⊑ˡ ✭ 
    ℓ⊑ℓ : ∀ {ℓ} → ℓ ⊑ˡ ℓ

_⊑ˡ?_ : ∀ (ℓ₁ ℓ₂ : Label) → Dec (ℓ₁ ⊑ˡ ℓ₂)
⊤ ⊑ˡ? ⊤ = yes ℓ⊑ℓ
⊤ ⊑ˡ? ⊥ = no (λ ())
⊤ ⊑ˡ? ✭ = yes ℓ⊑✭
⊥ ⊑ˡ? ⊤ = no (λ ())
⊥ ⊑ˡ? ⊥ = yes ℓ⊑ℓ
⊥ ⊑ˡ? ✭ = yes ℓ⊑✭
✭ ⊑ˡ? ⊤ = no (λ ())
✭ ⊑ˡ? ⊥ = no (λ ())
✭ ⊑ˡ? ✭ = yes ℓ⊑✭

-- type precision
data _⊑_ : (t₁ t₂ : GType) → Set where
    b⊑b : ∀ {ℓ₁ ℓ₂} → ℓ₁ ⊑ˡ ℓ₂ → bool ℓ₁ ⊑ bool ℓ₂
    λ⊑λ : ∀ {s₁₁ s₂₁ s₁₂ s₂₂ ℓ₁ ℓ₂} → s₁₁ ⊑ s₂₁ → s₁₂ ⊑ s₂₂ → ℓ₁ ⊑ˡ ℓ₂ → ((s₁₁ ⇒ ℓ₁) s₁₂) ⊑ ((s₂₁ ⇒ ℓ₂) s₂₂)

lem₁ : ∀ {t₁ t₂ t₃ t₄ ℓ ℓ₁} → (x : (t₁ ⇒ ℓ) t₂ ⊑ (t₃ ⇒ ℓ₁) t₄) → t₃ ⊑ t₁
lem₁ (λ⊑λ x x₁ x₂) = {!  !}

_⊑?_ : ∀ (t₁ t₂ : GType) → Dec (t₁ ⊑ t₂)
bool x ⊑? bool x₁ with x ⊑ˡ? x₁
bool x ⊑? bool x₁ | yes p = yes (b⊑b p)
bool x ⊑? bool x₁ | no ¬p = no (¬p ∘ lem)
    where lem : ∀ {ℓ₁ ℓ₂ : Label} → (bool ℓ₁ ⊑ bool ℓ₂) → ℓ₁ ⊑ˡ ℓ₂
          lem (b⊑b x) = x
bool x ⊑? (t₂ ⇒ x₁) t₃      = no (λ ())
bool x ⊑? err               = no (λ ())
(t₁ ⇒ x) t₂ ⊑? bool x₁      = no (λ ())
(t₁ ⇒ ℓ) t₂ ⊑? (t₃ ⇒ ℓ₁) t₄ with t₃ ⊑? t₁ | t₂ ⊑? t₄
(t₁ ⇒ ℓ) t₂ ⊑? (t₃ ⇒ ℓ₁) t₄ | yes p | (yes p₁) = yes {!   !}
(t₁ ⇒ ℓ) t₂ ⊑? (t₃ ⇒ ℓ₁) t₄ | yes p | (no ¬p) = no (¬p ∘ lem)
    where lem : ∀ {t₁ t₂ t₃ t₄ ℓ ℓ₁} (x : (t₁ ⇒ ℓ) t₂ ⊑ (t₃ ⇒ ℓ₁) t₄) → t₂ ⊑ t₄
          lem (λ⊑λ x x₁ x₂) = x₁
(t₁ ⇒ ℓ) t₂ ⊑? (t₃ ⇒ ℓ₁) t₄ | no ¬p | (yes p) = no {!!} 
(t₁ ⇒ ℓ) t₂ ⊑? (t₃ ⇒ ℓ₁) t₄ | no ¬p | (no ¬p₁) = no (¬p ∘ {!   !})
(t₁ ⇒ x) t₂ ⊑? err          = no (λ ())
err ⊑? bool x               = no (λ ())
err ⊑? (t₂ ⇒ x) t₃          = no (λ ())
err ⊑? err                  = no (λ ())
