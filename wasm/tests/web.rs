#![cfg(target_arch = "wasm32")]

use wasm_bindgen_test::{wasm_bindgen_test, wasm_bindgen_test_configure};

wasm_bindgen_test_configure!(run_in_browser run_in_worker);

#[wasm_bindgen_test]
fn pass() {
    assert_eq!(1 + 1, 2);
}
