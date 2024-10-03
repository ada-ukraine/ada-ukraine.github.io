Записи
======

.. include:: ../../global.txt

Наразі всі типи, з якими ми стикалися, мають значення, які не можна розкласти
на складники: кожен екземпляр представляє прості значення. Тепер ми побачимо
наш перший клас композитних типів: записи.

Записи дозволяють включати екземпляри інших типів. Кожному з цих екземплярів
буде присвоєно назву. Пара, що складається з імені та екземпляра певного типу,
називається полем або компонентом запису.

.. _Intro_Ada_Record_Type_Declaration:

Декларація типу-запису
----------------------

Ось приклад простої декларації запису:

.. code-block:: ada

    type Date is record
       --  Наступні декларації є
       --  компонентами запису
       Day   : Integer range 1 .. 31;
       Month : Months;
       --  Ви можете додати власні
       --  обмеження на компоненти
       Year  : Integer range 1 .. 3000;
    end record;

Поля запису дуже схожі на декларацію змінних, за винятком того, що вони
знаходяться всередині декларації запису. І, як і у випадку з декларацією
змінних, ви можете вказати додаткові обмеження під час декларації поля.

.. code-block:: ada

    type Date is record
       Day   : Integer range 1 .. 31;
       Month : Months := January;
       --  Наступне поле має значення
       --  за замовчуванням
       Year  : Integer range 1 .. 3000 := 2012;
       --                                 ^^^^
       --                               Значення
    end record;

.. _Intro_Ada_Record_Default_Values:

Компоненти запису можуть мати значення за замовчуванням. Коли створимо змінну
цього запису, поле з значенням за замовчуванням буде автоматично ініціалізовано
цим значенням. Значення може бути будь-яким виразом який відповідає типу даних
компонента та може бути таким що обчислюється під час виконання.

У решті розділів цієї глави ми побачимо, як використовувати типи записів.
Крім того, ми поговоримо більше про записи в :doc:`ще одному розділі <./more_about_records>`.


.. _Intro_Ada_Record_Aggregates:

Агрегати
--------

.. code-block:: ada

    --  Компоненти за порядком
    Ada_Birthday    : Date := (10, December, 1815);

    --  Компоненти за іменами
    Leap_Day_2020   : Date := (Day   => 29,
                               Month => February,
                               Year  => 2020);
    --                         ^ Імена

Записи мають зручну нотацію для вираження значень, проілюстровану вище.
Це називається агрегатним позначенням, а літерали — агрегатними. Їх можна
використовувати в різних контекстах, які ми побачимо протягом усього курсу,
один із яких – ініціалізація записів.

Агрегат — це список значень, розділених комами та взятих у круглі дужки.
Це дозволено в будь-якому контексті, де очікується значення запису.

Значення для компонентів можна вказати позиційно, як у прикладі
:ada:`Ada_Birthday`, або за назвою, як у :ada:`Leap_Day_2020`.
Допускається поєднання позиційних та іменованих значень, але ви не можете
використовувати позиційне позначення після іменованого.

Доступ до компонентів
---------------------

Щоб отримати доступ до компонентів екземпляра запису, ви використовуєте
операцію, яка називається вибором компонента. Це досягається використанням
нотації з крапкою. Наприклад, якщо ми створимо змінну :ada:`Some_Day` типу
запису :ada:`Date`, згаданого вище, ми зможемо отримати доступ до компонента
:ada:`Year`, написавши :ada:`Some_Day.Year`.

Поглянемо на приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Records.Record_Selection

    with Ada.Text_IO; use Ada.Text_IO;

    procedure Record_Selection is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Display_Date (D : Date) is
       begin
          Put_Line ("Day:" & Integer'Image (D.Day)
                    & ", Month: "
                    & Months'Image (D.Month)
                    & ", Year:"
                    & Integer'Image (D.Year));
       end Display_Date;

       Some_Day : Date := (1, January, 2000);

    begin
       Display_Date (Some_Day);

       Put_Line ("Changing year...");
       Some_Day.Year := 2001;

       Display_Date (Some_Day);
    end Record_Selection;

Як ви бачите в цьому прикладі, ми можемо використовувати крапкову нотацію
у виразі :ada:`D.Year` або :ada:`Some_Day.Year` для доступу до інформації,
що зберігається в цьому полі, а також для зміни цієї інформації.
Точніше кажучи, коли ми використовуємо :ada:`D.Year` у виклику :ada:`Put_Line`,
ми отримуємо інформацію, що зберігається в цьому полі. Коли ми пишемо
:ada:`Some_Day.Year := 2001`, ми перезаписуємо інформацію, яка раніше
зберігалася в полі :ada:`Year` змінної :ada:`Some_Day`.

.. _Intro_Ada_Record_Comp_Renaming:

Переіменування
--------------

У попередніх розділах ми обговорювали перейменування :ref:`підпрограм <Intro_Ada_Subprogram_Renaming>`
і :ref:`пакетів <Intro_Ada_Package_Renaming>`. Ми також можемо перейменувати
компоненти запису. Замість того, щоб писати повний вибір компонента за допомогою
крапкової нотації, ми можемо оголосити псевдонім, який дозволяє нам отримати
доступ до того самого компонента. Це корисно, наприклад, для спрощення
коду підпрограми.

Ми можемо перейменувати компоненти запису, використовуючи ключове слово
:ada:`renames` в декларації змінної. Наприклад:

.. code-block:: ada

    Some_Day : Date;
    Y        : Integer renames Some_Day.Year;

Тут :ada:`Y` є псевдонімом, тому кожного разу, коли ми використовуємо :ada:`Y`,
ми насправді використовуємо компонент :ada:`Year` запису :ada:`Some_Day`.

Поглянемо на приклад:

.. code:: ada run_button project=Courses.Intro_To_Ada.Arrays.Record_Component_Renaming

    package Dates is

       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer range 1 .. 3000 := 2032;
       end record;

       procedure Increase_Month
        (Some_Day : in out Date);

       procedure Display_Month
         (Some_Day : Date);

    end Dates;

    with Ada.Text_IO; use Ada.Text_IO;

    package body Dates is

       procedure Increase_Month
         (Some_Day : in out Date)
       is
          --  Переіменування компонентів
          --  запису Date
          M : Months  renames Some_Day.Month;
          Y : Integer renames Some_Day.Year;

          --  Переіменування функції (для 
          --  перечислимого типу Months)
          function Next (M : Months)
                         return Months
            renames Months'Succ;
       begin
          if M = December then
             M := January;
             Y := Y + 1;
          else
             M := Next (M);
          end if;
       end Increase_Month;

       procedure Display_Month
         (Some_Day : Date)
       is
          --  Переіменування компонентів
          --  запису Date
          M : Months  renames Some_Day.Month;
          Y : Integer renames Some_Day.Year;
       begin
          Put_Line ("Month: "
                    & Months'Image (M)
                    & ", Year:"
                    & Integer'Image (Y));
       end Display_Month;

    end Dates;

    with Ada.Text_IO; use Ada.Text_IO;
    with Dates;       use Dates;

    procedure Main is
       D : Date := (1, January, 2000);
    begin
       Display_Month (D);

       Put_Line ("Increasing month...");
       Increase_Month (D);

       Display_Month (D);
    end Main;

Ми застосовуємо перейменування до двох компонентів запису :ada:`Date`
у реалізації процедури :ada:`Increase_Month`. Тоді, замість прямого
використання :ada:`Some_Day.Month` і :ada:`Some_Day.Year` у наступних
операціях, ми просто використовуємо перейменовані версії :ada:`M`
і :ada:`Y`.

Зауважте, що у прикладі вище ми також перейменовуємо :ada:`Months'Succ`
|mdash| це функція, яка повертає нам наступний місяць |mdash|.
