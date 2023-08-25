(module component 
  (import base))

⍝ The Component class works as follows 
⍝ Component parameters
⍝ Query ◂ τ → τ (sent by parent whenever they like)
⍝   • handle with handle-query 
⍝   • handle-query (Query a) must return a value of type Maybe a
⍝ Input ◂ τ (sent by parent every call to render)
⍝   • handle with `receive` function
⍝ Output ◂ τ (can be sent by child in handle-action)
⍝   • Produce with `raise`
⍝ m

(Component ◂ τ → τ → τ)


(mk-component ◂ ∀ State Message. 
  (Σ (initial-state ◂ State)
     (render ◂ State → View Message)
     (eval ◂ Message → SilcM State ))
  → Component State Message)


⍝ View has type paramters
⍝ • Action: the messages that this component outputs
⍝ • Slots: 
⍝ • Monad: 
(View ◂ τ → τ → τ)

(View ) 