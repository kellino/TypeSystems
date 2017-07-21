module static where

open import Data.Nat using (ℕ; _+_; zero; suc)
open import Data.Fin using (Fin; toℕ; zero; suc)
open import Data.Vec using (lookup; _∷_; [])
open import Data.Bool using (Bool; true; false)
open import LSsyntax
open import Relation.Binary.PropositionalEquality -- using (_≡_; refl)


-- inference rules for typed terms
data STyped {n} (Γ : Ctx n) : GType → Set where
    Sx   : ∀ {S} (v : Fin n) → S ≡ lookup v Γ → STyped Γ S
    Sb   : Bool → (ℓ : Label) → STyped Γ (bool ℓ)
    _S∧_ : ∀ {t₁ t₂} → STyped Γ t₁ → STyped Γ t₂ → STyped Γ (bool (getLabel t₁ ~⋎~ getLabel t₂))
    _S∨_ : ∀ {t₁ t₂} → STyped Γ t₁ → STyped Γ t₂ → STyped Γ (bool (getLabel t₁ ~⋎~ getLabel t₂))
    Sif  : ∀ {t t₁ t₂} → STyped Γ t → STyped Γ t₁ → STyped Γ t₂ → STyped Γ (bool (getLabel (t₁ :∨: t₂) ~⋎~ getLabel t))
    S∙   : ∀ {t₁ ℓ t₂ t₃} → STyped Γ ((t₁ ⇒ ℓ) t₂) → STyped Γ t₃ → t₃ ≾ t₁ → STyped Γ (bool ((getLabel t₂) ~⋎~ ℓ))
    Sλ   : ∀ t₁ {t₂} ℓ → STyped (t₁ ∷ Γ) t₂ → STyped Γ ((t₁ ⇒ ℓ) t₂)

erase : ∀ {n} {Γ : Ctx n} {t} → STyped Γ t → Term
erase (Sx v x)      = var (toℕ v)
erase (Sb b ℓ)      = litBool b ℓ
erase (t₁ S∧ t₂)    = (erase t₁) ∧ erase t₂
erase (t₁ S∨ t₂)    = (erase t₁) ∨ (erase t₂)
erase (Sif b t₁ t₂) = if (erase b) then (erase t₁) else (erase t₂)
erase (S∙ t₁ t₂ _)  = (erase t₁) ∙ (erase t₂)
erase (Sλ t₁ ℓ t)   = lam t₁ (erase t) ℓ

data Fromℕ (n : ℕ) : ℕ → Set where
    yes : (m : Fin n) → Fromℕ n (toℕ m)
    no  : (m : ℕ) → Fromℕ n (n + m)

fromℕ : ∀ n m → Fromℕ n m
fromℕ zero m = no m
fromℕ (suc n) zero = yes zero
fromℕ (suc n) (suc m) with fromℕ n m
fromℕ (suc n) (suc .(toℕ m)) | yes m = yes (suc m)
fromℕ (suc n) (suc .(n + m)) | no m = no m

data Check {n} (Γ : Ctx n) : Term → Set where
    yes : (τ : GType) (t : STyped Γ τ) → Check Γ (erase t)
    no  : {e : Term} → Check Γ e

staticCheck : ∀ {n} (Γ : Ctx n) (t : Term) → Check Γ t
staticCheck {n} Γ (var v) with fromℕ n v 
staticCheck {n} Γ (var .(toℕ m)) | yes m = yes (lookup m Γ) (Sx m refl)
staticCheck {n} Γ (var .(n + m)) | no m = no

staticCheck Γ (litBool x ℓ) = yes (bool ℓ) (Sb x ℓ)

staticCheck Γ (lam x t ℓ) with staticCheck (x ∷ Γ) t
staticCheck Γ (lam x .(erase t) ℓ) | yes τ t = yes ((x ⇒ ℓ) τ) (Sλ x ℓ t)
staticCheck Γ (lam x t ℓ) | no = no

staticCheck Γ (t ∧ t₁) with staticCheck Γ t | staticCheck Γ t₁
staticCheck Γ (.(erase t₁) ∧ .(erase t)) | yes τ₁ t₁ | (yes τ t) = yes (bool (getLabel τ₁ ~⋎~ getLabel τ)) (t₁ S∧ t)
staticCheck Γ (.(erase t) ∧ t₁) | yes τ t | no = no
staticCheck Γ (t₁ ∧ .(erase t)) | no | yes τ t = no
staticCheck Γ (t ∧ t₁) | no | no = no

staticCheck Γ (t ∨ t₁) with staticCheck Γ t | staticCheck Γ t₁
staticCheck Γ (.(erase t₁) ∨ .(erase t)) | yes τ₁ t₁ | (yes τ t) = yes (bool (getLabel τ₁ ~⋎~ getLabel τ)) (t₁ S∨ t)
staticCheck Γ (.(erase t) ∨ t₁) | yes τ t | no = no
staticCheck Γ (t₁ ∨ .(erase t)) | no | yes τ t = no
staticCheck Γ (t ∨ t₁) | no | no = no

staticCheck Γ (t ∙ t₁) with staticCheck Γ t | staticCheck Γ t₁ 
staticCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ) τ₂) t | (yes τ₁ t₁) with τ₁ ≾? τ
staticCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ) τ₂) t | (yes τ₁ t₁) | (yes .τ₁ .τ) = yes (bool (getLabel τ₂ ~⋎~ ℓ)) (S∙ t t₁ (yes τ₁ τ))
staticCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ) τ₂) t | (yes τ₁ t₁) | (no .τ₁ .τ) = no
staticCheck Γ (.(erase t) ∙ .(erase t₁)) | yes err t | (yes τ₁ t₁) = no
staticCheck Γ (.(erase t) ∙ .(erase t₁)) | yes (bool x) t | (yes τ₁ t₁) = no
staticCheck Γ (.(erase t) ∙ t₁) | yes τ t | no = no
staticCheck Γ (t₁ ∙ .(erase t)) | no | yes τ t = no
staticCheck Γ (t ∙ t₁) | no | no = no

staticCheck Γ (if b then t₁ else t₂) with staticCheck Γ b
staticCheck Γ (if .(erase t) then t₁ else t₂) | yes τ t with staticCheck Γ t₁ | staticCheck Γ t₂
staticCheck Γ (if .(erase t₂) then .(erase t₁) else .(erase t)) | yes τ₂ t₂ | (yes τ₁ t₁) | (yes τ t) = yes (bool (getLabel (τ₁ :∨: τ) ~⋎~ getLabel τ₂)) (Sif t₂ t₁ t)
staticCheck Γ (if .(erase t₁) then .(erase t) else t₂)          | yes τ₁ t₁ | (yes τ t)   | no = no
staticCheck Γ (if .(erase t₂) then t₁ else .(erase t))          | yes τ₁ t₂ | no          | (yes τ t) = no
staticCheck Γ (if .(erase t) then t₁ else t₂)                   | yes τ t   | no          | no = no
staticCheck Γ (if b then t₁ else t₂)                            | no = no

staticCheck Γ error = no

-- examples from Section 3.5

-- Type : yes ((bool ⊥ ⇒ ⊥) (bool ⊥)) (Sλ (bool ⊥) ⊥ (Sx zero refl S∧ Sx zero refl))
f : Term
f = lam (bool ⊥) (var 0 ∨ var 0) ⊥

-- Type : yes ((bool ✭ ⇒ ⊥) (bool ✭)) (Sλ (bool ✭) ⊥ (Sx zero refl S∨ Sx zero refl))
g : Term
g = lam (bool ✭) (var 0 ∧ var 0) ⊥

-- Type : yes (bool ⊤) (Sb false ⊤)
v : Term
v = litBool false ⊤

test₁ : staticCheck [] (f ∙ v) ≡ no
test₁ = refl


