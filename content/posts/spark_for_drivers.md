---
date: Feb 16, 2026
author: Elsa Ferrara
tags: drivers, SPARK, embedded
---

# Формальний доказ на драйверах пристроїв за допомогою SPARK

> Переклад статті [Formal Proof on Device Drivers with SPARK](https://www.adacore.com/blog/formal-proof-on-device-drivers-with-spark)  
> Aug 16, 2023

Програмування драйверів пристроїв вимагає певних практик або операцій. 
До них відноситься, наприклад, велика кількість `volatile` змінних
у коді. Ці `Volatile` змінні необхідні для читання/запису апаратних
регістрів, тож вони можуть бути змінені зовнішнім середовищем програми.

Одночас, SPARK накладає низку обмежень на код програми, а також обмежує
використання деяких практик, дозволених в Ada. Через те, що вимоги SPARK
здаються несумісними з цим типом коду, необхідно виконати
«SPARKіфікацію»,  — перехід до коду, що відповідає стандартам SPARK.

У цій статті я висвітлю проблеми, з якими зіткнулася під час стажування,
адаптуючи код драйверів під правила, дозволені SPARK. Я також пропоную
способи обходу виявлених проблем, а також деякі найкращі практики для
написання максимально сумісного зі SPARK коду заздалегідь. Цей список
не є вичерпним.

Для вашої інформації, приклади, наведені в цьому дописі в блозі,
є фрагментами коду з бібліотеки, на якій я базувала свою роботу під
час стажування, створеної
[Jeremy Grosser](https://github.com/JeremyGrosser/rp2040_hal).
Заради ясності деякі частини коду були скорочені для кращого
розуміння. Ви можете знайти той самий
[код після внесених мною змін тут](https://github.com/elsaferrara/rp2040_hal).

## Volatile об'єкти

Як згадувалося у вступі, `Volatile` змінні дуже часто присутні в коді
драйверів, але SPARK дозволяє лише дуже обмежене поводження з ними.
Розглянемо наступний код як приклад, де `XOSC_Periph.CTRL.STABLE`
є `Volatile` змінною.

```ada
procedure Enable_XOSC is
begin
   XOSC_Periph.CTRL.FREQ_RANGE := Val_1_15MHZ;
   XOSC_Periph.CTRL.ENABLE := ENABLE;
   while not XOSC_Periph.STATUS.STABLE loop
      null;
   end loop;
end Enable_XOSC;
```

Цей фрагмент коду може здатися правильним, але він не відповідає правилам
SPARK. Дійсно, якщо ми подивимося на
[довідник SPARK, розділ 7.1.3 (10)](https://docs.adacore.com/spark2014-docs/html/lrm/packages.html#external-state-variables-and-types),
він говорить нам:

> Є загальне правило SPARK про те, що обчислення виразу не може мати
> побічних ефектів, але читання volatile об'єкта вважається таким,
> що має побічний ефект. Щоб узгодити цю невідповідність, ім'я, що
> позначає такий об'єкт, має зустрічатися лише в неінтерферуючому контексті.

Хоча команда SPARK розглядає можливість послаблення цих обмежень, щоб
дозволити й оригінальний код, наразі ми маємо це подолати.

Ми бачимо, що використання volatile змінної в такому вигляді не відповідає
вимозі «неінтерферуючого контексту». Тому необхідно перетворити код,
щоб він відповідав цьому правилу. Ось випадок, дозволений довідником:

> Ім'я зустрічається в неінтерферуючому контексті, якщо це: \[...\]
> вираз \[правої частини\] оператора присвоєння.

Для обходу обмеження достатньо присвоїти значення volatile об'єкта
тимчасовій змінній, яку потім можна використовувати.

```ada
procedure Enable_XOSC is
   Tmp : Boolean;
begin
   XOSC_Periph.CTRL.FREQ_RANGE := Val_1_15MHZ;
   XOSC_Periph.CTRL.ENABLE := ENABLE;
   loop
      Tmp := XOSC_Periph.STATUS.STABLE;
      exit when Tmp;
   end loop;
end Enable_XOSC;
```

Також існує обмеження на оголошення `Volatile` змінних, як показано
в наступному коді.

```ada
function Transmit_Status (This : UART_Port)
  return UART_FIFO_Status
is
   Flags : constant UARTFR_Register := This.Periph.UARTFR;
begin
   if Flags.TXFE = False and Flags.TXFF = False then
      return Not_Full;
   elsif Flags.TXFE = False and Flags.TXFF = True then
      return Full;
   elsif Flags.BUSY then
      return Busy;
   elsif Flags.TXFE = True and Flags.TXFF = False then
      return Empty;
   else
      return Invalid;
   end if;
end Transmit_Status;
```

У SPARK volatile змінні можна оголошувати лише  на рівні бібліотеки,
тож ми не можемо оголосити змінну `Flags` таким чином. Ось що пояснює
довідник у
[розділі 7.1.3 (3)](https://docs.adacore.com/spark2014-docs/html/lrm/packages.html#external-state-variables-and-types):

> Оголошення фактично volatile окремого об'єкта або типу має бути
> оголошенням на рівні бібліотеки. \[Зокрема, воно не повинно
> оголошуватися всередині підпрограми.\]

Отже, об'єкт потрібно оголосити в бібліотеці, а не в програмі.
У нашому випадку це неможливо, оскільки він залежить від параметра функції.
Щоб уникнути цієї проблеми, нам потрібно присвоїти кожне можливе значення
`Flags` змінній, щоб ми могли повторно використовувати його пізніше.

```ada
procedure Transmit_Status_Inner
  (Periph : UART_Peripheral;
   Result : out UART_FIFO_Status)
is
   Is_TXFE : constant Boolean := Periph.UARTFR.TXFE;
   Is_TXFF : constant Boolean := Periph.UARTFR.TXFF;
   Is_BUSY : constant Boolean := Periph.UARTFR.BUSY;
begin
   if not Is_TXFE and not Is_TXFF then
      Result := Not_Full;
   elsif not Is_TXFE and Is_TXFF then
      Result := Full;
   elsif Is_BUSY then
      Result := Busy;
   elsif Is_TXFE and not Is_TXFF then
      Result := Empty;
   else
      Result := Invalid;
   end if;
end Transmit_Status_Inner;
```

Останній момент щодо `Volatile` полягає в тому, що функція не може
використовувати `Volatile` змінні, крім випадку коли змінна є лише
“вхідною” змінною, а функція анотована за допомогою `"Volatile_Function"`. 
Наприклад, як у цьому прикладі, де `CLOCKS_Periph` є `Volatile` записом.

```ada
function Enabled (CID : Clock_Id) return Boolean
  is (CLOCKS_Periph.CLK (CID).CTRL.ENABLE)
    with Volatile_Function;
```

## Використання глобальних параметрів


Інша проблема сумісності зі SPARK — це використання глобальних параметрів.
Як ви, можливо, знаєте (або ні), функції та процедури не однакові;
відмінність між ними полягає в тому, що функція повертає значення,
а процедура — ні. У SPARK різниця ще сильніша, оскільки функції не
дозволено мати побічні ефекти (ніякої модифікації змінних).

```ada
function Frequency
  (CID      : Countable_Clock_Id;
   Rounded  : Boolean := True;
   Accuracy : UInt4 := 15) return Hertz
is
   F : Hertz;
begin
[...]
   if CLOCKS_Periph.FC0_STATUS.DIED then
      return 0;
   else
      F := Hertz (CLOCKS_Periph.FC0_RESULT.KHZ) * 1_000;
      if Rounded then
         return F;
      else
         return F +
		   Hertz (CLOCKS_Periph.FC0_RESULT.FRAC) * 3125 / 100;
      end if;
    end if;
```

Ось приклад простої функції. Спочатку вона здається сумісною з обмеженням
SPARK, але оскільки `CLOCKS_Periph` є глобальним параметром, цій підпрограмі
не дозволяється бути функцією через побічний ефект, про який я згадувала
раніше. Тому ми повинні перетворити її на процедуру. У деяких випадках це
може бути легко, але іноді вам потрібен результат функції, наприклад,
у передумовах, тому адаптація стає складнішою.

Наступний фрагмент має ту саму функціональність, що й код вище, але реалізує
її як процедуру, що повертає значення вихідним (`out`) параметром.

```ada
procedure Frequency
  (CID      : Countable_Clock_Id;
   Result : out Hertz;
   Rounded  : Boolean := True;
   Accuracy : UInt4 := 15)
is
   F : Hertz;
   Tmp : Boolean;
   Tmp2 : HAL.UInt25;
   Tmp3 : HAL.UInt5;
begin
   [...]      
   Tmp := CLOCKS_Periph.FC0_STATUS.DIED;
   if Tmp then
      Result := 0;
   else
      Tmp2 := CLOCKS_Periph.FC0_RESULT.KHZ;
      F := Hertz (Tmp2) * 1_000;
      if Rounded then
         Result := F;
      else
         Tmp3 := CLOCKS_Periph.FC0_RESULT.FRAC;
         Result := F + Hertz (Tmp3) * 3125 / 100;
      end if;
   end if;
 end Frequency;
```

Якщо ви плануєте, щоб ваш код був валідований у SPARK, майте на увазі,
що глобальні змінні можуть бути проблематичними, тому намагайтеся
максимально обмежити використання функцій.


## Використання покажчиків

Написання драйверів пристроїв іноді передбачає читання та доступ до
кількох екземплярів периферійного пристрою, і в таких випадках може
бути цікаво використовувати покажчики (`pointers`). Покажчики не
є великою проблемою, оскільки вони дозволені в SPARK, але не тоді,
коли змінна є `Volatile`, а `Volatile` змінні, як я вже казала,
є невід'ємною частиною коду пристрою.


Як приклад, ми можемо взяти цю функцію, яка використовує екземпляри
`PLL_SYS_Periph` та `PLL_USB_Periph` типу `PLL_Peripheral` за допомогою
типу доступу (`access type`).

```ada
procedure Configure_PLL
  (PLL    : PLL_Clock_Id;
   Config : PLL_Config)
is
   Periph : constant access PLL_Peripheral :=
     (if PLL = PLL_SYS then PLL_SYS_Periph'Access
      else PLL_USB_Periph'Access);
begin
   [...]
end Configure_PLL;
```

SPARK видає помилку, оскільки обидві змінні є `Volatile`. Щоб уникнути
цієї проблеми, ми вводимо допоміжну процедуру, де змінна, на яку спочатку
вказував покажчик, є додатковим аргументом.


Таким чином, ми можемо викликати внутрішню процедуру з правильними
параметрами залежно від потрібного значення.

```ada
procedure Configure_PLL
  (PLL    : PLL_Clock_Id;
   Config : PLL_Config)
is
   procedure Inner_Config
     (Periph : in out PLL_Peripheral) is
   begin
      [...]
   end Inner_Config;
begin
   if PLL = PLL_SYS then
      Inner_Config (PLL_SYS_Periph);
   else
      Inner_Config (PLL_USB_Periph);
   end if;
end Configure_PLL;
```

Зауважте, що ми використовували цей трюк для портів SPI та UART, щоб
викликати правильні периферійні пристрої. Це було можливо, оскільки було
лише два порти для кожного протоколу.


Ви також можете побачити нижче, як були змінені оголошення портів, щоб
уникнути використання покажчиків. Замість покажчика ми даємо правильну
змінну залежно від потрыбного порту.

```ada
SPI_0 : aliased RP.SPI.SPI_Port (0, RP2040_SVD.SPI.SPI0_Periph'Access);
SPI_1 : aliased RP.SPI.SPI_Port (1, RP2040_SVD.SPI.SPI1_Periph'Access);
-- нове оголошення   
SPI_0 : aliased RP.SPI.SPI_Port (0);
SPI_1 : aliased RP.SPI.SPI_Port (1);

procedure Enable_IRQ
  (This : SPI_Port;
   IRQ  : SPI_IRQ_Flag)
is
   procedure Enable_IRQ_Inner
     (IRQ    : SPI_IRQ_Flag;
      Periph : in out SPI_Peripheral) is
   begin
      [...]
   end Enable_IRQ_Inner;
begin
   case This.Num is
      when 0 => Enable_IRQ_Inner (IRQ, SPI0_Periph);
      when 1 => Enable_IRQ_Inner (IRQ, SPI1_Periph);
   end case;
end Enable_IRQ;
```

Тож це можливо, оскільки портів небагато. Тут ми бачимо обмеження
і те, як може бути важко не використовувати покажчики (`access`).


Ці обмеження також призвели до того, що ми змінили значну частину
програми, видаливши абстракцію HAL, яка була присутня спочатку.
Дійсно, типи доступу до інтерфейсів наразі не підтримуються в SPARK.
Тому для цілей цього стажування ми вирішили видалити цю абстракцію,
що може бути проблематично в контексті більшого проекту, де код буде
множитися.

## Адреса

Специфікація адресу (`Address)` дозволена за певних умов, але у випадку
з прикладу нижче, оскільки об'єкт не є повністю ініціалізований при
оголошенні.

```ada
AUXSRC : CLK_CTRL_AUXSRC_Field
  with
    Volatile,
    Address => CLOCKS_Periph.CLK (GP).CTRL.AUXSRC'Address;
```

У цьому випадку, щоб уникнути проблем зі SPARK, рекомендується
перейменувати змінну, оскільки вона відповідає тому самому полю.
Чим менше пунктів `address` ви використовуєте, тим менше проблем
ви зустрінете.

```ada
AUXSRC : CLK_CTRL_AUXSRC_Field renames
  CLOCKS_Periph.CLK (GP).CTRL.AUXSRC;
```

Якщо перейменування неможливе, найкраще уникати доступу через
адресу значення, коли це можливо. Якщо адреса є обов'язковою,
використовуйте `SPARK_Mode => Off`.


## Числові типи та перетворення

Основна рекомендація полягає в тому, що тип даних має бути якомога
точнішим. Навіть якщо це не має прямого впливу, це значно полегшує
процес доказу за допомогою SPARK. У царені драйверів, де дані часто
обмежені обладнанням або середовищем, зазвичай точні типи вказати легко.


Також рекомендується бути обережним з перетворенням типів, оскільки
неправильне перетворення може призвести до пошкодження цілісності даних.
Наприклад, SPARK поки що не дозволяє перетворення між типами з фіксованою
та плаваючою точкою. Використання неперевірених перетворень
(`Ada.Unchecked_Conversion`) має бути обмеженим, а розмір обох типів
повинен бути явно вказаний та бути однаковим.

## Висновок

Отже, ми розглянули кілька хитрощів для адаптації існуючого низькорівневого
коду до SPARK та узгодження обмежень SPARK з потребами коду драйверів.
Хоча SPARK накладає певні обмеження на програмування драйверів пристроїв,
він надає можливість створювати безпечний і надійний код. Приймаючи виклики
та дотримуючись найкращих практик, розробники можуть досягти сумісних
зі SPARK рішень, які забезпечують надійність та покращують загальну
якість їхніх проектів.

**Автор:** [Ельза Феррара](https://www.adacore.com/blog/author/elsa-ferrara)


