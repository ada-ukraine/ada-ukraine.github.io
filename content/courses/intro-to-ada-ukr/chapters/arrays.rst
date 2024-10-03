Массиви
=======

.. include:: ../../global.txt

Массиви забезпечують ще одне фундаментальне сімейство композитних типів в Ada.

.. _Intro_Ada_Array_Type_Declaration:

Декларація типу массива
-----------------------

Массиви в Ada використовуються для визначення безперервних колекцій елементів,
які можна вибрати шляхом індексування. Ось простий приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index is range 1 .. 5;

       type My_Int_Array is
         array (Index) of My_Int;
       --                 ^ Тип єлементів
       --       ^ Межі массива
       Arr : My_Int_Array := (2, 3, 5, 7, 11);
       --                    ^ Агрегат

       V : My_Int;
    begin
       for I in Index loop
          V := Arr (I);
          --        ^ Взяти елемент
          --          по індексу І
          Put (My_Int'Image (V));
       end loop;
       New_Line;
    end Greet;

Перше, на що слід звернути увагу, це те, що ми визначаємо тип індексу для массиву,
а не його розмір. Тут ми оголосили цілочисельний тип з іменем :ada:`Index` в
діапазоні від :ada:`1` до :ada:`5`, тому кожен екземпляр массиву матиме 5 елементів,
з початковим елементом за індексом 1 і останнім елементом за індексом 5.

Хоча в цьому прикладі для індексу використовується цілочисельний тип, Ada є більш
загальною: будь-якому дискретному типу дозволено індексувати массив, включаючи
:ref:`Перечислимий тип <Intro_Ada_Enum_Types>`. Незабаром ми побачимо, що це означає.

Ще один момент, на який слід звернути увагу, полягає в тому, що для досткпк до
елемента массиву за заданим індексом використовується той самий синтаксис, що й
для викликів функцій: тобто об’єкт массиву, за яким слідує індекс у дужках.

Таким чином, коли ви бачите такий вираз, як :ada:`A (B)`, чи є це виклик функції
чи індекс массиву, залежить від того, що таке :ada:`A`.

Нарешті, зверніть увагу на те, як ми ініціалізуємо массив за допомогою виразу
:ada:`(2, 3, 5, 7, 11)`. Це ще один вид агрегату в Ada, і в певному сенсі він є
буквальним виразом для массиву, так само, як :ada:`3` є буквальним виразом для
цілого числа. Нотація є дуже потужною, з низкою властивостей, які ми представимо
пізніше. Детальний огляд з’являється в нотації :ref:`агрегатні типи <Intro_Ada_Aggregates>`.

В прикладі вище також проілюстровано два виклики процедур з :ada:`Ada.Text_IO` не пов’язані
із массивами:

*  :ada:`Put`, яка виводить строку на екран без переходу на нову строку

*  :ada:`New_Line`, те саме але з переходом на нову строку

Давайте тепер заглибимося в те, що означає мати можливість використовувати будь-який
дискретний тип для індексування массиву.

.. admonition:: В інших мовах

    Семантично об’єкт массиву в Ada — це сама структура даних, а не просто
    дескриптор чи вказівник. На відміну від C і C++, між массивом і вказівником
    на його початковий елемент немає неявної еквівалентності.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Array_Bounds_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Array_Bounds_Example is
       type My_Int is range 0 .. 1000;

       type Index is range 11 .. 15;
       --                  ^ Нижня межа може
       --                    бути будь якою

       type My_Int_Array is
         array (Index) of My_Int;

       Tab : constant My_Int_Array :=
               (2, 3, 5, 7, 11);
    begin
       for I in Index loop
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Array_Bounds_Example;

Одним з ефектів є те, що межі массиву можуть мати будь-які значення.
У першому прикладі ми створили тип массиву, перший індекс якого :ada:`1`,
але в наведеному вище прикладі ми декларуємо тип массиву, перший індекс
якого :ada:`11`.

Це цілком нормально в Ada, і, крім того, оскільки ми використовуємо тип для
індексу як діапазон для перебору індексів массивую Такий код не потребує змін
у разі змін меж типу.

Це приводить нас до важливого наслідку щодо коду, що працює з массивами.
Оскільки межі можуть змінюватися, вам не слід використовувати конкретні
межі для ітерації. Це означає, що наведений вище код правильний, оскільки
він використовує тип для індексу, але цикл for, який наведено нижче, є
поганою практикою, навіть якщо він і працює правильно зараз:

.. code-block:: ada

    for I in 11 .. 15 loop
       Tab (I) := Tab (I) * 2;
    end loop;

Оскільки ви можете використовувати будь-який дискретний тип для індексування
массиву, перечислимі типи дозволені.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Month_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Month_Example is
       type Month_Duration is range 1 .. 31;
       type Month is (Jan, Feb, Mar, Apr,
                      May, Jun, Jul, Aug,
                      Sep, Oct, Nov, Dec);

       type My_Int_Array is
         array (Month) of Month_Duration;
       --       ^ Може використовуватись
       --         перечислимий тип як індекс

       Tab : constant My_Int_Array :=
       --    ^ константа схожа на змінну але її
       --      значення не можна змінювати
         (31, 28, 31, 30, 31, 30,
          31, 31, 30, 31, 30, 31);
       --  Відповідність кількості днів для
       --   місцяців (без високосного)

       Feb_Days : Month_Duration := Tab (Feb);
       --  Кількість днів в лютому
    begin
       for M in Month loop
          Put_Line
            (Month'Image (M) & " has "
             & Month_Duration'Image (Tab (M))
             & " days.");
       --    ^ Оператор об'еднання строк
       end loop;
    end Month_Example;

В прикладі више:

- Створення типу массиву, що відображає кількість днів у місяцях.

- Створення екземпляру массиву та ініціалізація шляхом зіставлення
  місяців з їх фактичною тривалістю в днях.

- Ітерація по массиву, друк місяців і кількості днів для кожного.

Можливість використовувати переречислимі типи як індекси дуже корисна
для створення відображень, як показано вище, і є часто використовуваною
функцією в Ada.

Індексація
----------

Ми вже бачили синтаксис вибору елементів массиву. Однак є ще кілька
моментів, на які слід звернути увагу.

По-перше, як і взагалі в Ada, операція індексування суворо типізована.
Якщо ви використовуєте значення невідповідного типу для індексування массиву,
ви отримаєте помилку під час компіляції.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_2
    :class: ada-expect-compile-error

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;

       type My_Index   is range 1 .. 5;
       type Your_Index is range 1 .. 5;

       type My_Int_Array is
         array (My_Index) of My_Int;

       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Your_Index loop
          Put (My_Int'Image (Tab (I)));
       --                         ^ Помилка компіляції
       end loop;
       New_Line;
    end Greet;

По-друге, масcиви в Ada перевіряються на межі. Це означає, що якщо ви спробуєте
отримати доступ до елемента поза межами масcиву, ви отримаєте помилку під час
виконання замість доступу до довільної пам’яті, як у мовах без такої перевірки.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_3
    :class: ada-run-expect-failure

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       type My_Int is range 0 .. 1000;
       type Index  is range 1 .. 5;

       type My_Int_Array is
         array (Index) of My_Int;

       Tab : My_Int_Array := (2, 3, 5, 7, 11);
    begin
       for I in Index range 2 .. 6 loop
          Put (My_Int'Image (Tab (I)));
          --                      ^ Викличе виключення
          --                        коли I = 6
       end loop;
       New_Line;
    end Greet;

Простіша декларація массиву
---------------------------

У попередніх прикладах ми завжди явно вказували тип індексу для массиву.
Хоча це може бути корисним для ясності коду, інколи потрібно просто вказати
діапазон значень. Ada дозволяє вам це робити також.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Simple_Array_Bounds

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Simple_Array_Bounds is
       type My_Int is range 0 .. 1000;

       type My_Int_Array is
         array (1 .. 5) of My_Int;
       --       ^ цілочисленний підтип

       Tab : constant My_Int_Array :=
               (2, 3, 5, 7, 11);
    begin
       for I in 1 .. 5 loop
       --       ^ цілочисленний підтип
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Simple_Array_Bounds;

Цей приклад визначає діапазон массиву за допомогою синтаксису діапазону,
який визначає анонімний підтип Integer і використовує його для індексування
массиву.

Це означає, що тип індексу :ada:`Integer`. Так само, коли ви використовуєте
анонімний діапазон у циклі for, як у прикладі вище, тип змінної ітерації також
:ada:`Integer`, тому ви можете використовувати :ada:`I` для індексування :ada:`Tab `.

Ви також можете використовувати іменований підтип для меж массиву.

.. _Intro_Ada_Range_Attribute:

Атрибут Range
-------------

Раніше ми зазначали, що використання конкретних значень в коді при ітерації
массиву є поганою ідеєю, і показали, як використовувати тип/підтип індексу
массиву для повторення його діапазону в циклі :ada:`for`. Це піднімає
питання про те, як написати ітерацію, коли массив має анонімний діапазон
для своїх меж, оскільки немає імені для посилання на діапазон. Ada вирішує
це за допомогою кількох атрибутів об’єктів массиву:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Range_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Range_Example is
       type My_Int is range 0 .. 1000;

       type My_Int_Array is
         array (1 .. 5) of My_Int;

       Tab : constant My_Int_Array :=
               (2, 3, 5, 7, 11);
    begin
       for I in Tab'Range loop
       --          ^ Отримуємо межі Tab
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Range_Example;

Якщо вам потрібен більш детальний контроль, ви можете використовувати інші
атрибути :ada:`'First` і :ada:`'Last`.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Array_Attributes_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Array_Attributes_Example is
       type My_Int is range 0 .. 1000;

       type My_Int_Array is
         array (1 .. 5) of My_Int;

       Tab : My_Int_Array :=
               (2, 3, 5, 7, 11);
    begin
       for I in Tab'First .. Tab'Last - 1 loop
       --          ^ Ітерація по кожному елементу
       --            окрім останнього
          Put (My_Int'Image (Tab (I)));
       end loop;
       New_Line;
    end Array_Attributes_Example;

Атрибути :ada:`'Range`, :ada:`'First` і :ada:`'Last` у цих прикладах також
можна було застосувати до назви типу массиву, а не лише до екземпляру массиву.

Хоча це не показано у наведених вище прикладах, іншим корисним атрибутом
екземпляра массиву :ada:`A` є :ada:`A'Length`, що повертає кількість елементів,
які містить :ada:`A`.

Це законно, а іноді і корисно мати "нульовий массив", який не містить елементів.
Щоб отримати такий необхідно створити діапазон індексів, верхня межа якого менша
за нижню.

.. _Intro_Ada_Unconstrained_Array_Types:

Необмеженні массиви
-------------------

Давайте тепер розглянемо один із найпотужніших аспектів інструменту массиву Ada.

Кожен тип масcиву, який ми декларували досі, має фіксований розмір: кожен
екземпляр цього типу матиме однакові межі, а отже, однакову кількість елементів
і однаковий розмір.

Однак Ada також дозволяє декларувати типи массивів, межі яких не є фіксованими:
у такому випадку межі потрібно буде визначити під час створення екземплярів
такого массиву.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Unconstrained_Array_Example

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Unconstrained_Array_Example is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       type Workload_Type is
         array (Days range <>) of Natural;
       --  Необмеженний тип массиву
       --       ^ Межі мають тип Days,
       --         але не визначені
       --         конкретними значеннями

       Workload : constant
         Workload_Type (Monday .. Friday) :=
       --               ^ Вказуємо межі при
       --                 створенні екземпляру
          (Friday => 7, others => 8);
       --               ^ Значення за замовчуванням
       --  ^ елемент вказаний по назів
    begin
       for I in Workload'Range loop
          Put_Line (Integer'Image (Workload (I)));
       end loop;
    end Unconstrained_Array_Example;

Те, що межі массиву невідомі, вказує синтаксис :ada:`Days range <>`.
Враховуючи дискретний тип :ada:`Discrete_Type`, якщо ми використовуємо
:ada:`Discrete_Type` як індекс в типі массиву, тоді :ada:`Discrete_Type`
служить типом індексу та містить діапазон значень індексу для кожного
екземпляру массиву.

Якщо ми визначаємо індекс як :ada:`Discrete_Type range <>`, тоді
:ada:`Discrete_Type` буде типом індексу, але різні екземпляри массиву
можуть мати різні межі відповідно до меж типу індексу.

Тип массиву, який визначено за допомогою синтаксису :ada:`Discrete_Type range <>`
для його індексу, називається необмеженим типом массиву, і, як показано вище,
межі потрібно вказувати під час створення екземпляра.

Наведений вище приклад також демонструє інші форми агрегатного синтаксису.
Ви можете вказати на елемент массиву, вказавши значення індексу ліворуч від
асоціації зі стрілкою.
:ada:`1 => 2` таким чином означає:
"встановити значення 2 елементу з індексом 1 у моєму массиві".
:ada:`others => 8` означає:
"встановити значення 8 кожному елементу, який раніше не був згаданий у цьому агрегаті".

.. attention::
    Так звана нотація "коробка" (:ada:`<>`) зазвичай використовується як символ
    підстановки або заповнювач в Ada. Ви часто побачите це, коли значення
    "те, що тут очікується, може бути будь-чим".

.. admonition:: В інших мовах

    Хоча необмеженні массиви в Ada можуть здаватися схожими на массиви змінної
    довжини в C, насправді вони є набагато потужнішими, оскільки насправді
    відіграють роль першокласних значеннь. Ви можете передати їх як параметри
    підпрограмам або повернути їх із функцій, і вони неявно мають свої межі
    як частину свого значення. Це означає, що немає необхідності передавати
    межі або довжину массиву явно разом із массивом, оскільки вони доступні
    через :ada:`'First`, :ada:`'Last`, :ada:`'Range` і :ada:`'Length` атрибути,
    пояснені раніше.

.. _Intro_Ada_Unconstrained_Array_Type_Instance_Bound:

Хоча різні екземпляри одного необмеженого типу массиву можуть мати різні межі,
певний екземпляр має ті самі межі протягом усього часу існування. Це дозволяє
Ada ефективно впроваджувати необмежені массиви; екземпляри можуть зберігатися
в стеку і не потребують виділення пам'яті з купи, як у таких мовах, як Java.

Попередньо визначений тип массиву: String
-----------------------------------------

Тема, що повторює ідею з нашого вступу до типів Ada, полягає в тому, що
вбудовані типи, такі як :ada:`Boolean` або :ada:`Integer`, визначаються
за допомогою тих самих засобів, які доступні для користувача.
Це також вірно для строк: тип :ada:`String` в Ada є простим массивом.

Ось як вона визначається в Ada:

.. code-block:: ada

    type String is
      array (Positive range <>) of Character;

Єдина вбудована функція, яку Ada додає, щоб зробити строки більш ергономічними,
це літерали, як ми можемо бачити в прикладі нижче.

.. hint::
    Строкові літерали синтаксично є агрегатами, тому в наступному
    прикладі :ada:`A` і :ada:`B` мають однакові значення.

    .. code:: ada no_button project=Courses.Intro_To_Ada.Arrays.String_Literals

        package String_Literals is
            --  Ці дві строки еквівалентні
            A : String (1 .. 11) := "Hello World";
            B : String (1 .. 11) :=
                ('H', 'e', 'l', 'l', 'o', ' ',
                 'W', 'o', 'r', 'l', 'd');
        end String_Literals;

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_4

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : String (1 .. 11) := "dlroW olleH";
       --        ^ Попередньо визначений тип.
       --          Елементом є тип Character
    begin
       for I in reverse Message'Range loop
          --    ^ Ітерація в зворотньому порядку
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

Однак явно вказувати межі об’єкта – це трохи клопотно; вам потрібно вручну
підрахувати кількість символів у літералі. На щастя, Ada пропонує вам простіший шлях.

Ви можете не вказувати межі під час створення екземпляра необмеженого типу массиву,
якщо ви ініціалізуєте його, оскільки межі можна вирахувати з виразу ініціалізації.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Greet_5

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Greet is
       Message : constant String := "dlroW olleH";
       --                 ^ Межі автоматично
       --                   вираховані з
       --                   ініціалізаційного
       --                   значення
    begin
       for I in reverse Message'Range loop
          Put (Message (I));
       end loop;
       New_Line;
    end Greet;

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Constant_Integer_Array

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
       type Integer_Array is
         array (Natural range <>) of Integer;

       My_Array : constant Integer_Array :=
                    (1, 2, 3, 4);
       --         ^^^^^^^^^^^^^^^^^^^^^
       --          ^ Межі автоматично
       --            вираховані з
       --            ініціалізаційного
       --            значення
    begin
        null;
    end Main;

.. attention::
    Як ви бачили вище, стандартним типом :ada:`String` в Ada є массив.
    Таким чином, він розділяє переваги та недоліки массивів: екземпляри
    :ada:`String` розподіляються в стек, доступ до нього ефективний,
    а його межі постійні.

    Якщо вам потрібне щось схоже на C++ :cpp:`std::string`, ви можете
    використати :ref:`Необмежені строки <Intro_Ada_Unbounded_Strings>`
    зі стандартної бібліотеки Ada. Цей тип більше схожий на автоматично
    керований строковий буфер, до якого можна додавати вміст.

Обмеження
---------

Дуже важливий момент щодо массивів: *повинні* бути відомі межі під час створення
екземплярів. Наприклад, наступне робити неможна.

.. code-block:: ada

    declare
       A : String;
       --  ^ Неможливо вирахувати
       --    межі
    begin
       A := "World";
    end;

Крім того, хоча Ви, звичайно, можете змінювати значення елементів у массиві,
ви не можете змінити межі массиву (і, отже, його розмір) після його ініціалізації.
Отже, наступне теж неможливо:

.. code-block:: ada

    declare
       A : String := "Hello";
    begin
       A := "World";       --  OK: Той самий розмір
       A := "Hello World"; --  Помилка: Розміри різні
    end;

Крім того, хоча Ви можете очікувати попередження про помилку такого роду
в дуже простих випадках, як цей, компілятор не може знати в загальному випадку,
чи ви призначаєте значення правильної довжини, тому це порушення, як правило,
призведе до помилки під час виконання.

.. _Intro_Ada_Indefinite_Subtype:

.. admonition:: Увага

    Хоча ми дізнаємося більше про це пізніше, важливо знати,
    що массиви — це не єдині типи, екземпляри яких можуть мати
    невідомий розмір під час компіляції.

    Кажуть, що такі об’єкти мають *невизначений підтип*, що означає, що
    розмір підтипу невідомий під час компіляції, але обчислюється динамічно
    (під час виконання).

    .. code:: ada no_button project=Courses.Intro_To_Ada.Arrays.Indefinite_Subtypes

        with Ada.Text_IO; use Ada.Text_IO;

        procedure Indefinite_Subtypes is
            function Get_Number return Integer is
            begin
                return Integer'Value (Get_Line);
            end Get_Number;

           A : String := "Hello";
           --  Невизначений підтип

           B : String (1 .. 5) := "Hello";
           --  Визначений підтип

           C : String (1 .. Get_Number);
           --  Невизначений підтип
           --  (значення Get_Number визначається
           --   під час виконання)
        begin
           null;
        end Indefinite_Subtypes;

   Тут атрибут :ada:`'Value` перетворює тип-строку на тип-цілочисленне


Повернення необмежених массивів
-------------------------------

Тип результату функції може бути будь-яким; функція може повернути значення,
розмір якого невідомий під час компіляції. Так само параметри можуть бути
будь-якого типу.

Наприклад, наступна функція повертає необмежений :ada:`String`:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Day_Name_1

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is

       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       function Get_Day_Name (Day : Days := Monday)
                              return String is
       begin
          return
            (case Day is
             when Monday    => "Monday",
             when Tuesday   => "Tuesday",
             when Wednesday => "Wednesday",
             when Thursday  => "Thursday",
             when Friday    => "Friday",
             when Saturday  => "Saturday",
             when Sunday    => "Sunday");
       end Get_Day_Name;

    begin
       Put_Line ("First day is "
                 & Get_Day_Name (Days'First));
    end Main;

(Цей приклад лише для ілюстрації. Існує вбудований механізм, атрибут :ada:`'Image`
для скалярних типів, який повертає назву (як :ada:`String`) будь-якого елемента
переліку. Наприклад, :ada:`Days'Image (Monday)` це :ada:`"MONDAY"`.)

.. admonition:: В інших мовах

    Повернення об’єктів змінного розміру в мовах, у яких відсутній збирач сміття,
    є дещо складним у реалізації, тому C і C++ не дозволяють це робити, віддаючи
    перевагу явному динамічному розподілу / вільному від користувача.

    Проблема полягає в тому, що явне керування пам'ятью небезпечно, як тільки Ви
    хочете звільнити пам’ять яка вже не використовується. Здатність Ada повертати
    об’єкти змінного розміру усуне цей варіант використання динамічного розподілу,
    а отже, усуне одне з потенційних джерел помилок у Ваших програмах.

    Rust дотримується моделі C/C++, але з безпечною семантикою вказівника. Однак
    динамічний розподіл все ще використовується. Ada може виграти в продуктивності,
    оскільки вона може використовувати будь-яку модель.

Декларація массивів (2)
-----------------------

Хоча ми можемо мати типи массивів, розмір і межі яких визначаються під час виконання,
тип едемента массиву повинен мати певний і обмежений тип.

Таким чином, якщо вам потрібно декларувати, наприклад, массив строк, підтип :ada:`String`,
який використовується як едемент, повинен мати фіксований розмір.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Day_Name_2

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Show_Days is
       type Days is (Monday, Tuesday, Wednesday,
                     Thursday, Friday,
                     Saturday, Sunday);

       subtype Day_Name is String (1 .. 2);
       --  Підтип строки з відомими межами

       type Days_Name_Type is
         array (Days) of Day_Name;
       --       ^ Тип індексу
       --                ^ Тип едементу.
       --                  Має бути визначеним

       Names : constant Days_Name_Type :=
         ("Mo", "Tu", "We", "Th", "Fr", "Sa", "Su");
       --  Ініціалізаційне значення як агрегат
    begin
       for I in Names'Range loop
          Put_Line (Names (I));
       end loop;
    end Show_Days;

Фрагменти массиву
-----------------

Остання особливість массивів Ada, яку ми збираємося охопити, це фрагменти массиву.
Можна взяти та використати фрагмент массиву (безперервну послідовність елементів)
як назву або значення.

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Slices

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Main is
        Buf : String := "Hello ...";

        Full_Name : String := "John Smith";
    begin
        Buf (7 .. 9) := "Bob";
        --  Обережно! Це працює тому що строка
        --  зправа має таку саму довжину як
        --  замішувана частина!

        --  Виведе "Hello Bob"
        Put_Line (Buf);

         --  Виведе "Hi John"
        Put_Line ("Hi " & Full_Name (1 .. 4));
    end Main;

Як ми бачимо вище, ви можете використовувати фрагмент ліворуч від присвоєння,
щоб замінити лише частину массиву.

Фрагмент массиву має той самий тип, що й массив, але має інший підтип,
обмежений межами фрагмента.

.. attention::
    Ада має :arm:`багатовимірні масиви <3-6>`, які не розглядаються в цьому
    курсі. Фрагменти працюватимуть лише на одновимірних массивах.

.. _Intro_Ada_Object_Renaming:

Переіменування
--------------

Наразі ми побачили, що такі елементи можна перейменувати:
:ref:`підпрограми <Intro_Ada_Subprogram_Renaming>`, :ref:`пакети <Intro_Ada_Package_Renaming>`,
і :ref:`компоненти записів <Intro_Ada_Record_Comp_Renaming>`.
Ми також можемо перейменовувати об’єкти за допомогою ключового слова :ada:`renames`.
Це дозволяє створювати альтернативні імена для цих об’єктів. Давайте розглянемо приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Variable_Renaming

    package Measurements is

       subtype Degree_Celsius is Float;

       Current_Temperature : Degree_Celsius;

    end Measurements;

    with Ada.Text_IO;  use Ada.Text_IO;
    with Measurements;

    procedure Main is
       subtype Degrees is
         Measurements.Degree_Celsius;

       T : Degrees
             renames Measurements.Current_Temperature;
    begin
        T := 5.0;

        Put_Line (Degrees'Image (T));
        Put_Line (Degrees'Image
          (Measurements.Current_Temperature));

        T := T + 2.5;

        Put_Line (Degrees'Image (T));
        Put_Line (Degrees'Image
          (Measurements.Current_Temperature));
    end Main;

У наведеному вище прикладі ми оголошуємо змінну :ada:`T`, перейменувавши об’єкт
:ada:`Current_Temperature` з пакету :ada:`Measurements`. Як ви можете побачити,
виконавши цей приклад, і :ada:`Current_Temperature`, і його альтернативна назва
:ada:`T` мають однакові значення:

- спочатку вони показують значення 5,0
- після додавання вони показують значення 7,5.

Це пояснюється тим, що вони, по суті, посилаються на той самий об’єкт, але з
двома різними назвами.

Зауважте, що у прикладі вище ми використовуємо :ada:`Degrees` як псевдонім
:ada:`Degree_Celsius`. Ми обговорювали цей метод :ref:`раніше <Intro_Ada_Subtype_Aliases>`.

Перейменування може бути корисним для покращення читабельності складного індексування массиву.
Замість того, щоб явно використовувати індекси кожного разу, коли ми звертаємося до певних
позицій массиву, ми можемо створити коротші імена для цих позицій, перейменувавши їх.
Давайте розглянемо наступний приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Reverse_Colors

    package Colors is

       type Color is (Black,
                      Red,
                      Green,
                      Blue,
                      White);

       type Color_Array is
         array (Positive range <>) of Color;

       procedure Reverse_It (X : in out Color_Array);

    end Colors;

    package body Colors is

       procedure Reverse_It (X : in out Color_Array)
       is
       begin
          for I in X'First ..
                   (X'Last + X'First) / 2
          loop
             declare
                Tmp     : Color;
                X_Left  : Color
                  renames X (I);
                X_Right : Color
                  renames X (X'Last + X'First - I);
             begin
                Tmp     := X_Left;
                X_Left  := X_Right;
                X_Right := Tmp;
             end;
          end loop;
       end Reverse_It;

    end Colors;

    with Ada.Text_IO; use Ada.Text_IO;

    with Colors; use Colors;

    procedure Test_Reverse_Colors is

       My_Colors : Color_Array (1 .. 5) :=
         (Black, Red, Green, Blue, White);

    begin
       for C of My_Colors loop
          Put_Line ("My_Color: "
                    & Color'Image (C));
       end loop;

       New_Line;
       Put_Line ("Reversing My_Color...");
       New_Line;
       Reverse_It (My_Colors);

       for C of My_Colors loop
          Put_Line ("My_Color: "
                    & Color'Image (C));
       end loop;

    end Test_Reverse_Colors;

У наведеному вище прикладі пакет :ada:`Colors` реалізує процедуру :ada:`Reverse_It`
шляхом оголошення нових імен для двох позицій массиву. Фактичну реалізацію стає
легко читати:

.. code-block:: ada

    begin
       Tmp     := X_Left;
       X_Left  := X_Right;
       X_Right := Tmp;
    end;

Порівняйте це з альтернативною версією без перейменування:

.. code-block:: ada

    begin
       Tmp                      := X (I);
       X (I)                    := X (X'Last +
                                   X'First - I);
       X (X'Last + X'First - I) := Tmp;
    end;
