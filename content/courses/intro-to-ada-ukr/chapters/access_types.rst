Тип-посилання
=============

.. include:: ../../global.txt

.. _Intro_Ada_Access_Types_Overview:

Огляд
-----

Росилання є потенційно небезпечною конструкцією, яка суперечить основній
філософії Ади.

Існує два шляхи, якими Ada допомагає захистити програмістів від небезпек
посилань:

1. Одним із підходів, який ми вже бачили, є надання альтернативних функцій,
   щоб програміст не потребував використання посилань. Режими параметрів,
   масиви та типи змінних розмірів — це конструкції, які можуть замінити
   типове використання покажчиків у C.

2. По-друге, Ada зробила посилання максимально безпечними та обмеженими, але
   дозволяє «аварійний режим», коли програміст явно їх вимагає, і, ймовірно,
   використовуватиме такі функції з належною обережністю.

..
   Цей курс охоплює основи посилань в Ada. Загалом є кращі способи, ніж
   вдаватися безпосередньо до розширених функцій, але якщо вам потрібно
   використовувати функції, які є потенційно небезпечними, ви можете
   дізнатися більше про ці небезпечні функції ACCESS_TYPES_ADVANCED_LINK.

Приклад декрарації простого типу посилання в Ada:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    package Dates is
       type Months is
         (January, February, March, April,
          May, June, July, August, September,
          October, November, December);

       type Date is record
          Day   : Integer range 1 .. 31;
          Month : Months;
          Year  : Integer;
       end record;
    end Dates;

    with Dates; use Dates;

    package Access_Types is
        --  Декларація типу посилання
        type Date_Acc is access Date;
        --                      ^ Date_Acc значення
        --                        посилається на
        --                        обєкт Date

        D : Date_Acc := null;
        --              ^ Літерал для
        --                "немає посилання"
        --  ^ Посилання на дату
    end Access_Types;

Це ілюструє, як:

- Декларує тип-посилання, значення якого вказують на об'єкти певного типу
- Декларує змінну цього типу
- Ініціалізує значенням :ada:`null`

Відповідно до філософії суворої типізації Ada, якщо ви оголосите ще один
тип-посилання та тип Date, ці типи будуть несумісні один з одним:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types
    :class: ada-expect-compile-error

    with Dates; use Dates;

    package Access_Types is
        --  Декларація типів-посилань
        type Date_Acc   is access Date;
        type Date_Acc_2 is access Date;

        D  : Date_Acc   := null;
        D2 : Date_Acc_2 := D;
        --                 ^ Невірно! Різні типи
    end Access_Types;

.. admonition:: В інших мовах

    У більшості інших мов типи-посилання є структурно, а не номінально
    типізованими, як це є в Ada, що означає, що два типи посилань будуть
    однаковими, доки вони мають однаковий цільовий тип і правила доступності.

    В Аді це не так. Потрібен час, щоб звикнути до цього. Здавалося б, проста
    проблема: якщо ви хочете мати канонічний доступ до типу, де його слід оголосити?
    Зазвичай використовуваний шаблон полягає в тому, що якщо вам потрібен тип-посилання
    на певний тип, яким ви «володієте», ви повинні оголосити його разом із типом:

    .. code-block:: ada

        package Access_Types is
           type Point is record
              X, Y : Natural;
           end record;

           type Point_Access is access Point;
        end Access_Types;

Виділення пам'яті
-----------------

Після того як ми декларували тип-посилання, нам потрібен спосіб надати
змінним значення. Ви можете призначити значення тиакм змінним за допомогою
ключового слова :ada:`new`.

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
        type Date_Acc is access Date;

        D : Date_Acc := new Date;
        --              ^ Виділення пам'яті для Date
    end Access_Types;

.. _Intro_Ada_Access_Type_Allocation_Constraints:

Якщо тип, для якого хочете виділити пам'ять, потребує обмежень, ви можете
вказати їх при виділенні, так само, як ви зробили б у декларації змінної:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type String_Acc is access String;
       --                        ^
       --  Посилання на необмеженний тип
       Msg : String_Acc;
       --    ^ За замовчуванням значення null

       Buffer : String_Acc :=
         new String (1 .. 10);
       --            ^ Необхідно вказати обмеження
    end Access_Types;

У деяких випадках, однак, створення з вказанням обмежень не є ідеальним,
тому Ada також дозволяє ініціалізувати при виділенні. Наприклад:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;
       type String_Acc is access String;

       D   : Date_Acc   :=
               new Date'(30, November, 2011);
       Msg : String_Acc := new String'("Hello");
    end Access_Types;


.. _Intro_Ada_Access_Dereferencing:

Доступ до значення
------------------

Останньою важливою частиною функції типу-посилання в Ada є те, як отримати
доступ до значення на якє посилається такий тип, тобто як розіменувати посилання.
Розіменування посилання використовує синтаксис :ada:`.all` в Ada, але зазвичай
це не потрібне |mdash| у багатьох випадках воно буде виконано неявно для вас:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Access_Types

    with Dates; use Dates;

    package Access_Types is
       type Date_Acc is access Date;

       D     : Date_Acc :=
                 new Date'(30, November, 2011);

       Today : Date := D.all;
       --              ^ Доступ до значення
       J     : Integer := D.Day;
       --                 ^ Неявне розіменування
       --                   для записів і массивів
       --                   Еквівалентно до D.all.Day
    end Access_Types;

Інші можливості
---------------

Як ви могли знати, якщо ви використовували посилання в C або C++, нам все ще
бракує функцій, які вважаються фундаментальними для використання посилань,
наприклад:

- Арифметичні дії (здатність збільшувати або зменшувати посилання, щоб
  вказати на наступний або попередній об’єкт)

- Звільнення пам'яті вручну — те, що в C називається :c:`free` або :c:`delete`.
  Це потенційно небезпечна операція.

Ці функції існують в Ada, але доступні лише через спеціальні API стандартної
бібліотеки.


..
   Ви можете прочитати більше про них у поглибленому курсі керування
   пам’яттю ACCESS_TYPES_ADVANCED_LINK.

.. attention::

    Правило Ada полягає в тому, що в більшості випадків ви можете
    уникнути ручного розподілу, і Ви повинні.

    Існує багато способів уникнути розподілу вручну, деякі з яких розглянуто
    (наприклад, режими параметрів). Мова також надає бібліотечні абстракції,
    щоб уникнути посилань:

    1. Одним з них є використання :ref:`containers <Intro_Ada_Containers>`.
       Контейнери допомагають користувачам уникати посилань, оскільки пам’ять
       контейнера керується автоматично.

    2. Контейнер, на який слід звернути увагу в цьому контексті, це
       :rat12:`Indefinite holder <8-5>`. Цей контейнер дозволяє зберігати
       значення невизначеного розміру, наприклад String.

    3. Бібліотека GNATCOLL має інтелектуальні вказівники під назвою
       `Refcount <https://github.com/AdaCore/gnatcoll-core/blob/master/src/gnatcoll-refcount.ads>`_
       Пам’ять в них керується автоматично, тому, коли виділений об’єкт
       більше не має посилань на нього, пам’ять автоматично звільняється.

Взаємно рекурсивні типи
-----------------------

Зв'язаний список є загальною ідіомою в структурах даних; в Ada це було б найбільш
природно визначити через два типи, тип запису та тип-посилання, які є взаємозалежними.
Щоб оголосити взаємозалежні типи, ви можете використовувати неповне оголошення типу:

.. code:: ada compile_button project=Courses.Intro_To_Ada.Access_Types.Simple_List

    package Simple_List is
       type Node;
       --  Це не повна декларація типу,
       --  яка має бути повністю декларована
       --  в цьому ж декларативному блоці.

       type Node_Acc is access Node;

       type Node is record
          Content    : Natural;
          Prev, Next : Node_Acc;
       end record;
    end Simple_List;

У цьому прикладі типи :ada:`Node` і :ada:`Node_Acc` є взаємозалежними.
