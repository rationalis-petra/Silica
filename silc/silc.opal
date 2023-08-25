
(module silc)


(run-ui 
 ◂ ∀ query input output.
  Component query input output IO
  → input 
  → Gtk.Element
  → IO (SilcIO query output IO))


(SilcIO ◂ τ → τ → τ → τ)
(SilcIO query output m ≜ 
 Σ (query ◂ ∀ α. query α → m (Maybe a))
   (messages ◂ Event output)
   (dispose ◂ m Unit))