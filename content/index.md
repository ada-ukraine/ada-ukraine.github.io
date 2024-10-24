# Ada-Ukraine.github.io

## Що ж таке Ада?

Ада - це сучасна мова програмування, яку команди розробників по всьому
світу використовують для створення критично важливого програмного
забезпечення: від мікроядер і мініатюрних вбудованих систем реального
часу до масштабних корпоративних рішень і всього, що знаходиться між цим.

## Спробуй Аду саме зараз:

```{code} ada run_button project=Introduction main=learn.adb

with Ada.Text_IO; use Ada.Text_IO;

procedure Learn is

   subtype Alphabet is Character range 'A' .. 'Z';

begin

   Put_Line ("Learning Ada from " & Alphabet'First & " to " & Alphabet'Last);

end Learn;
```

`````{container} content-blocks

   ````{only} builder_html
      ```{toctree}

      Про Ada Ukraine <about>
      ```
   ````

   ```{toctree}
      :maxdepth: 1
      :caption: "Courses:"

      Курси <courses>
   ```

   ````{only} builder_html
      ```{toctree}

      Статті <posts>
      ```
   ````
`````
