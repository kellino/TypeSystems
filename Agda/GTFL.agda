module GTFL where

open import Data.Nat hiding (_⊓_; erase; _≟_; _≤_)
open import Data.Bool hiding (_≟_)
open import Data.Fin using (Fin; zero; suc; toℕ)
open import Data.Vec
open import Relation.Binary.PropositionalEquality
open import Relation.Nullary
open import Data.Empty
open import Function using (_∘_)

-- | Types
infixr 30 _⇒_
data GType : Set where
    nat  : GType
    bool : GType
    _⇒_  : GType → GType → GType
    ✭    : GType
    err  : GType    -- easier to model this as a type in Agda

-- | Untyped Expressions
data Expr : Set where
    litNat      : ℕ → Expr
    litBool     : Bool → Expr
    dyn         : Expr
    err         : Expr
    var         : ℕ → Expr
    lam         : GType → Expr → Expr
    _∙_         : Expr → Expr → Expr
    _⊕_         : Expr → Expr → Expr
    if_thn_els_ : Expr → Expr → Expr → Expr

Ctx : ℕ → Set
Ctx = Vec GType

infixr 10 _~_
data _~_ {A B : Set} (x : A) (y : B) : Set where
    cons : x ~ y

~dom : ∀ (t : GType) → GType
~dom (t ⇒ t₁) = t₁
~dom _ = err

~cod : ∀ (t : GType) → GType
~cod (t ⇒ t₁) = t₁
~cod _ = err

_⊓_ : ∀ (t₁ t₂ : GType) → GType
nat ⊓ nat = nat
bool ⊓ bool = bool
t₁ ⊓ ✭ = t₁
✭ ⊓ t₂ = t₂
(t₁ ⇒ t₂) ⊓ (t₃ ⇒ t₄) = (t₁ ⊓ t₃) ⇒ (t₂ ⊓ t₄)
_ ⊓ _ = err

-- | Typed Terms
data Term {n} (Γ : Ctx n) : GType → Set where
    Tx   : ∀ {t} (v : Fin n) → t ≡ lookup v Γ → Term Γ t
    Tn   : ℕ → Term Γ nat
    Tb   : Bool → Term Γ bool
    Tdy  : Term Γ ✭
    _T∙_ : ∀ {t₁ t₂} → Term Γ t₁ → Term Γ t₂ → t₂ ~ (~dom t₁) → Term Γ (~cod t₁)
    _T⊕_ : ∀ {t₁ t₂} → Term Γ t₁ → Term Γ t₂ → (t₁ ~ nat) → (t₂ ~ nat) → Term Γ (t₁ ⊓ t₂)
    Tif  : ∀ {t₁ t₂ t₃} → Term Γ t₁ → Term Γ t₂ → Term Γ t₃ → (t₁ ~ bool) → Term Γ (t₂ ⊓ t₃)
    Tlam : ∀ t₁ {t₂} → Term (t₁ ∷ Γ) t₂ → Term Γ (t₁ ⇒ t₂)

erase : ∀ {n} {Γ : Ctx n} {t} → Term Γ t → Expr
erase (Tx v x) = var (toℕ v)
erase (Tn x) = litNat x
erase (Tb x) = litBool x
erase Tdy    = dyn
erase ((term T∙ term₁) _) = (erase term) ∙ (erase term₁)
erase ((term T⊕ term₁) _ _) = (erase term) ⊕ (erase term₁)
erase (Tif b tt ff  _) = if erase b thn erase tt els erase ff
erase (Tlam t₁ term) = lam t₁ (erase term)

data Fromℕ (n : ℕ) : ℕ → Set where
    yes : (m : Fin n) → Fromℕ n (toℕ m)
    no  : (m : ℕ) → Fromℕ n (n + m)

fromℕ : ∀ n m → Fromℕ n m
fromℕ zero m = no m
fromℕ (suc n) zero = yes zero
fromℕ (suc n) (suc m) with fromℕ n m
fromℕ (suc n) (suc .(toℕ m)) | yes m = yes (suc m)
fromℕ (suc n) (suc .(n + m)) | no m = no m

data Check {n} (Γ : Ctx n) : Expr → Set where
    yes : (τ : GType) (t : Term Γ τ) → Check Γ (erase t)
    no  : {e : Expr} → Check Γ e

staticCheck : ∀ {n} (Γ : Ctx n) (t : Expr) → Check Γ t
-- | primitives
staticCheck Γ (litNat x)  = yes nat (Tn x)
staticCheck Γ (litBool x) = yes bool (Tb x)
staticCheck {n} Γ dyn     = yes ✭ Tdy
staticCheck Γ err         = no

-- | var lookup
staticCheck {n} Γ (var v) with fromℕ n v 
staticCheck {n} Γ (var .(toℕ m)) | yes m = yes (lookup m Γ) (Tx m refl)
staticCheck {n} Γ (var .(n + m)) | no m  = no

-- | lambda abstraction
staticCheck Γ (lam x t) with staticCheck (x ∷ Γ) t
staticCheck Γ (lam x .(erase t)) | yes τ t = yes (x ⇒ τ) (Tlam x t) -- double check this
staticCheck Γ (lam x t)          | no      = no

-- | application
staticCheck Γ (t₁ ∙ t₂) with staticCheck Γ t₁ | staticCheck Γ t₂ 
staticCheck Γ (.(erase t₁) ∙ .(erase t)) | yes (τ₁ ⇒ τ₂) t₁ | (yes τ t) = yes τ₂ ((t₁ T∙ t) cons)
staticCheck Γ (.(erase t₁) ∙ .(erase t)) | yes _ t₁         | (yes τ t) = no -- not sure about this
staticCheck Γ (t₁ ∙ t₂)                  | _                | _         = no

-- | addition
staticCheck Γ (t₁ ⊕ t₂) with staticCheck Γ t₁ | staticCheck Γ t₂ 
staticCheck Γ (.(erase t₁) ⊕ .(erase t)) | yes nat t₁ | (yes nat t) = yes nat ((t₁ T⊕ t) cons cons)
staticCheck Γ (.(erase t₁) ⊕ .(erase t)) | yes ✭ t₁ | (yes nat t) = yes (✭ ⊓ nat) ((t₁ T⊕ t) cons cons)
staticCheck Γ (.(erase t₁) ⊕ .(erase t)) | yes nat t₁ | (yes ✭ t) = yes (nat ⊓ ✭) ((t₁ T⊕ t) cons cons)
staticCheck Γ (.(erase t₁) ⊕ .(erase t)) | yes ✭ t₁ | (yes ✭ t) = yes ✭ ((t₁ T⊕ t) cons cons)
staticCheck Γ (t₁ ⊕ t₂)                  | _ | _ = no

-- | if ... then ... else
staticCheck Γ (if t thn t₁ els t₂) with staticCheck Γ t
staticCheck Γ (if .(erase t) thn t₁ els t₂) | yes bool t with staticCheck Γ t₁ | staticCheck Γ t₂
staticCheck Γ (if .(erase t₂) thn .(erase t₁) els .(erase t)) | yes bool t₂ | (yes τ₁ t₁) | (yes τ₂ t) = yes (τ₁ ⊓ τ₂) (Tif t₂ t₁ t cons)
staticCheck Γ (if .(erase t₁) thn .(erase t) els t₂) | yes bool t₁ | (yes τ t) | no = no
staticCheck Γ (if .(erase t₂) thn t₁ els .(erase t)) | yes bool t₂ | no | (yes τ t) = no
staticCheck Γ (if .(erase t) thn t₁ els t₂) | yes bool t | no | _ = no

staticCheck Γ (if .(erase t) thn t₁ els t₂) | yes ✭ t with staticCheck Γ t₁ | staticCheck Γ t₂
staticCheck Γ (if .(erase t₂) thn .(erase t₁) els .(erase t)) | yes ✭ t₂ | (yes τ₁ t₁) | (yes τ₂ t) = yes (τ₁ ⊓ τ₂) (Tif t₂ t₁ t cons)
staticCheck Γ (if .(erase t₁) thn .(erase t) els t₂)          | yes ✭ t₁ | (yes τ t) | no = no
staticCheck Γ (if .(erase t₂) thn t₁ els .(erase t))          | yes ✭ t₂ | no | (yes τ t) = no
staticCheck Γ (if .(erase t) thn t₁ els t₂)                   | yes ✭ t | no | no = no

staticCheck Γ (if .(erase t) thn t₁ els t₂) | yes _ t = no
staticCheck Γ (if t thn t₁ els t₂) | no               = no

extractType : ∀ {n} {Γ : Ctx n} {t : Expr} → Check Γ t → GType
extractType (yes τ t) = τ
extractType no = err

-- Type Precision
data _⊑_ : GType → GType → Set where
    n⊑✭ : nat ⊑ ✭
    b⊑✭ : bool ⊑ ✭
    ⇒⊑ : ∀ (t₁ t₂ : GType) → (t₁ ⇒ t₂) ⊑ ✭
    n⊑n : nat ⊑ nat
    b⊑b : bool ⊑ bool
    ✭⊑✭ : ✭ ⊑ ✭
    app⊑ : ∀ (t₁ t₂ t₃ t₄ : GType) → t₁ ⊑ t₃ → t₂ ⊑ t₄ → (t₁ ⇒ t₃) ⊑ (t₃ ⇒ t₄)

-- Term Precision
data _≤_ : Expr → Expr → Set where
    n≤n : ∀ {n} → litNat n ≤ litNat n
    b≤b : ∀ {b} → litBool b ≤ litBool b
    n≤✭ : ∀ {n} → litNat n ≤ dyn
    b≤✭ : ∀ {b} → litBool b ≤ dyn
    d≤d : dyn ≤ dyn

ssG : ∀ {n} {Γ : Ctx n} {e₁ e₂ : Expr} → e₁ ≤ e₂ → extractType (staticCheck Γ e₁) ⊑ extractType (staticCheck Γ e₂)
ssG n≤n = n⊑n
ssG b≤b = b⊑b
ssG n≤✭ = n⊑✭
ssG b≤✭ = b⊑✭
ssG d≤d = ✭⊑✭
