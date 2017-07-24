module dynamic where

open import LSsyntax
open import static 
open import Relation.Nullary
open import Data.Nat using (ℕ ; _+_)
open import Data.Fin using (Fin; toℕ)
open import Data.Vec using (Vec ; lookup; _∷_; [])
open import Data.Product
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Data.Bool using (Bool; true ; false)

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

I≼ : ∀ (ℓ₁ ℓ₂ : Label) → (Label × Label)
I≼ ⊤ ✭   = ⊤ , ⊤
I≼ ✭ ⊤   = ✭ , ⊤
I≼ ✭ ⊥   = ⊥ , ⊥
I≼ ⊥ ✭   = ⊥ , ✭
I≼ ℓ₁ ℓ₂ = ℓ₁ , ℓ₂

Δ≼ : ∀ (triple : Label × Label × Label) → (Label × Label)
Δ≼ (ℓ₁ , ⊤ , ⊤) = ℓ₁ , ⊤
Δ≼ (ℓ₁ , ⊤ , ✭) = ℓ₁ , ⊤
Δ≼ (⊥ , ⊥ , ℓ₃) = ⊥ , ℓ₃
Δ≼ (⊥ , ✭ , ℓ₃) = ⊥ , ℓ₃
Δ≼ (ℓ₁ , ✭ , ℓ₃) = ℓ₁ , ℓ₃
Δ≼ (ℓ₁ , ℓ₂ , ℓ₃) = ℓ₁ , ℓ₃

Δ< : ∀ (triple : GType × GType × GType) → (GType × GType)
Δ< (bool ℓ₁ , bool ℓ₂ , bool ℓ₃) = 
    let new = Δ≼ (ℓ₁ , ℓ₂ , ℓ₃) in 
    bool (proj₁ new) , bool (proj₂ new)
Δ< ((t₁ ⇒ ℓ₁) t′₁ , (t₂ ⇒ ℓ₂) t′₂ , (t₃ ⇒ ℓ₃) t′₃) = 
    let new = Δ≼ (ℓ₁ , ℓ₂ , ℓ₃) in
    ((t₁ ⇒ (proj₁ new)) t′₁) , ((t₃ ⇒ (proj₂ new)) t′₃)
Δ< (_ , _ , _) = err , err

interior : ∀ (t : GType) → (GType × GType)
interior (bool ℓ) =  (bool ℓ) , (bool ℓ)
interior ((t ⇒ ℓ) t₁) =
    let (ℓ₁ , ℓ₂) = I≼ (getLabel t) ℓ in
    ((setLabel t ℓ₁) ⇒ ℓ₂) t₁ , ((setLabel t ℓ₁) ⇒ ℓ₂) t₁
interior err = err , err

_∘<_ : ∀ (t₁ t₂ : (GType × GType)) → (GType × GType)
(s₁ , s₂₁) ∘< (s₂₂ , s₃) = Δ< (s₁ , (s₂₁ ⊓ s₂₂) , s₃)

dynamicCheck : ∀ {n} (Γ : Ctx n) (t : Term) → Check Γ t
-- variables
dynamicCheck {n} Γ (var v) with fromℕ n v
dynamicCheck {n} Γ (var .(toℕ m)) | yes m = yes (lookup m Γ) (Sx m refl)
dynamicCheck {n} Γ (var .(n + m)) | no m = no

-- literals
dynamicCheck Γ (litBool x ℓ) = yes (bool ℓ) (Sb x ℓ)

-- lambda abstraction
dynamicCheck Γ (lam x t x₁) with dynamicCheck (x ∷ Γ) t 
dynamicCheck Γ (lam x .(erase t) ℓ) | yes τ t = yes ((x ⇒ ℓ) τ) (Sλ x ℓ t)
dynamicCheck Γ (lam x t x₁) | no = no

-- logical and
dynamicCheck Γ (t ∧ t₁) with dynamicCheck Γ t | dynamicCheck Γ t₁
dynamicCheck Γ (.(erase t) ∧ .(erase t₁)) | yes τ t | (yes τ₁ t₁) with (interior τ) ∘< (interior τ₁)
dynamicCheck Γ (.(erase t) ∧ .(erase t₁)) | yes τ t | (yes τ₁ t₁) | (bool x , bool x₁) = yes (bool (getLabel τ ~⋎~ getLabel τ₁)) (t S∧ t₁)
dynamicCheck Γ (.(erase t) ∧ .(erase t₁)) | yes τ t | (yes τ₁ t₁) | (_ , _) = no
dynamicCheck Γ (t ∧ t₁) | _ | _ = no

-- logical or
dynamicCheck Γ (t ∨ t₁) with dynamicCheck Γ t | dynamicCheck Γ t₁
dynamicCheck Γ (.(erase t) ∨ .(erase t₁)) | yes τ t | (yes τ₁ t₁) with (interior τ) ∘< (interior τ₁) 
dynamicCheck Γ (.(erase t) ∨ .(erase t₁)) | yes τ t | (yes τ₁ t₁) | (bool x , bool x₁) = yes (bool (getLabel τ ~⋎~ getLabel τ₁)) (t S∨ t₁)
dynamicCheck Γ (.(erase t) ∨ .(erase t₁)) | yes τ t | (yes τ₁ t₁) | (_ , _) = no
dynamicCheck Γ (t ∨ t₁) | _ | _ = no

-- application
-- this needs to be doublechecked!
dynamicCheck Γ (t ∙ t₁) with dynamicCheck Γ t | dynamicCheck Γ t₁ 
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ₁) τ₁) t | yes τ₂ t₁ with (interior ((τ ⇒ ℓ₁) τ₁)) ∘< (interior τ₂) 
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ₁) τ₁) t | (yes τ₂ t₁) | (bool x , bool x₁) = yes (bool (getLabel τ₁ ~⋎~ ℓ₁)) (S∙ t t₁ (yes τ₂ τ))
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ₁) τ₁) t | (yes τ₂ t₁) | (bool x , (proj₄ ⇒ x₁) proj₅) = no
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ₁) τ₁) t | (yes τ₂ t₁) | ((proj₃ ⇒ x) proj₄ , bool x₁) = no
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ₁) τ₁) t | (yes τ₂ t₁) | ((proj₃ ⇒ x) proj₄ , (proj₅ ⇒ x₁) proj₆) = no
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes ((τ ⇒ ℓ₁) τ₁) t | (yes τ₂ t₁) | (_ , _) = no
dynamicCheck Γ (.(erase t) ∙ .(erase t₁)) | yes _ t | (yes _ t₁) = no
dynamicCheck Γ (.(erase t) ∙ t₁) | yes τ t | no = no
dynamicCheck Γ (t₁ ∙ .(erase t)) | no | yes τ t = no
dynamicCheck Γ (t ∙ t₁) | no | no = no

-- if then else
dynamicCheck Γ (if b then t₁ else t₂) with dynamicCheck Γ b
dynamicCheck Γ (if .(erase b) then t₁ else t₂) | yes τ b with dynamicCheck Γ t₁ | dynamicCheck Γ t₂
dynamicCheck Γ (if .(erase b) then .(erase t₁) else .(erase t₂)) | yes τ b | (yes τ₁ t₁) | (yes τ₂ t₂) with (interior τ₁) ∘< (interior τ₂)
dynamicCheck Γ (if .(erase b) then .(erase t₁) else .(erase t₂)) | yes τ b | (yes τ₁ t₁) | (yes τ₂ t₂) | (bool ℓ , bool ℓ₁) = yes (bool (getLabel (τ₁ :∨: τ₂) ~⋎~ getLabel τ)) (Sif b t₁ t₂)
dynamicCheck Γ (if .(erase b) then .(erase t₁) else .(erase t₂)) | yes τ b | (yes τ₁ t₁) | (yes τ₂ t₂) | (_ , _) = no
dynamicCheck Γ (if .(erase b) then t₁ else t₂) | yes τ b | _ | _ = no
dynamicCheck Γ (if b then t₁ else t₂) | no = no

-- error 
dynamicCheck Γ error = no
