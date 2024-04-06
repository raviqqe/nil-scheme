import { useStore } from "@nanostores/solid";
import { Boxes, CirclePlay } from "lucide-solid";
import { type JSX } from "solid-js";
import * as store from "../stores/demo";
import { Button } from "./Button";
import { ButtonGroup } from "./ButtonGroup";
import styles from "./DemoForm.module.css";
import { ErrorMessage } from "./ErrorMessage";
import { Label } from "./Label";
import { TextArea } from "./TextArea";
import { Field } from "./Field";

export const DemoForm = (): JSX.Element => {
  const source = useStore(store.source);
  const bytecodesReady = useStore(store.bytecodesReady);
  const compiling = useStore(store.compiling);
  const interpreting = useStore(store.interpretingStore);
  const error = useStore(store.compilerError);

  return (
    <form class={styles.container}>
      <Field style={{ flex: 1 }}>
        <Label for="source">Program</Label>
        <TextArea
          id="source"
          onChange={(source) => store.source.set(source)}
          style={{ flex: 1 }}
          value={source()}
        />
        <ErrorMessage>{error()}</ErrorMessage>
      </Field>
      <ButtonGroup>
        <Button disabled={compiling()} icon={<Boxes />} onClick={store.compile}>
          {compiling() ? "Compiling..." : "Compile"}
        </Button>
        <Button
          disabled={!bytecodesReady() || interpreting()}
          icon={<CirclePlay />}
          onClick={store.interpret}
        >
          {interpreting() ? "Interpreting..." : "Interpret"}
        </Button>
      </ButtonGroup>
    </form>
  );
};
