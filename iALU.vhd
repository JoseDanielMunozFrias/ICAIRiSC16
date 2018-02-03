-- ALU de 16 bits para la CPU ICAI-RiSC-16.
-- Dispone de dos operandos de entrada y una salida de 16 bits, un código de
-- operación de 4 bits y dos flags que indican errores.
-- Se ha hecho con tamaño genérico para poder validarla mejor por simulación
-- usando un menor número de bits.
--
-- Códigos de operación:
-- ADD  --> 0000 Suma
-- SUB  --> 0001 Resta
-- MUL  --> 0010 Multiplicación (resultado truncado a 16 bits)
-- MULU --> 0011 Multiplicación sin signo (16 bits)
-- NAND --> 0100 Por eso, la NAND
-- SLL  --> 0101 Desplazamiento lógico a izquierda
-- SRA  --> 0110 Desplazamiento arimético a derecha
-- SRL  --> 0111 Desplazamiento lógico a derecha
-- SLTU --> 1000 Set on Less Than Unsigned
-- SLT  --> 1001 Set on Less Than (Signed)
--
-- Flags de salida:
-- co   --> Carry out
-- ov   --> Overflow
-- z    --> Zero
--
-- Versión 1.1
-- Autor: Germán Ferreira Peña y José Daniel Muñoz Frías. ICAIdea.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ConstantesICAIRiSC16.all;

entity ALU is
  generic(
    t_d : integer := 16);               -- Tamaño del bus de datos

  port (
    a, b   : in  std_logic_vector(t_d-1 downto 0);  -- Argumentos de entrada
    op_alu : in  std_logic_vector(3 downto 0);      -- Código de operación
    res    : out std_logic_vector(t_d-1 downto 0);  -- Resultado
    co     : out std_logic;                         -- Acarreo de salida
    ov     : out std_logic;                         -- Desbordamiento
    z      : out std_logic);                        -- Cero

end ALU;

architecture Behavioural of ALU is
  signal res_sum  : std_logic_vector(t_d-1 downto 0);    -- Salida del sumador
  signal co_sum   : std_logic;          -- Carry out del sumador/restador
  signal ov_sum   : std_logic;          -- Overflow del sumador/restador
  signal res_mul  : std_logic_vector(2*t_d-1 downto 0);  -- Salida del multiplicador
  signal ov_mul   : std_logic;          -- Overflow del multiplicador
  signal res_mulu : std_logic_vector(2*t_d-1 downto 0);  -- Salida del mult. sin signo
  signal ov_mulu  : std_logic;          -- Overflow del mult. sin signo
  signal res_nand : std_logic_vector(t_d-1 downto 0);    -- Salida de la NAND
  signal res_sll  : std_logic_vector(t_d-1 downto 0);    -- Salida del SRL
  signal res_sra  : std_logic_vector(t_d-1 downto 0);    -- Salida del SRA
  signal res_srl  : std_logic_vector(t_d-1 downto 0);    -- Salida del SRL
  signal res_sltu : std_logic_vector(t_d-1 downto 0);    -- Salida del SLTU
  signal res_slt  : std_logic_vector(t_d-1 downto 0);    -- Salida del SLT

  -- Constantes para obtener el overflow.
  constant c_parte_alta_ceros : std_logic_vector(2*t_d-1 downto t_d) := (others => '0');
  constant c_parte_alta_unos  : std_logic_vector(2*t_d-1 downto t_d) := (others => '1');
  
  component SumadorRestador
    generic (
      t_d : integer);
    port (
      s_r : in  std_logic;
      a   : in  std_logic_vector (t_d-1 downto 0);
      b   : in  std_logic_vector (t_d-1 downto 0);
      co  : out std_logic;
      ov  : out std_logic;
      res : out std_logic_vector (t_d-1 downto 0));
  end component;

begin  -- Behavioural

  -- El sumador/restador, dado que es relativamente complejo se crea en un
  -- componente y se instancia aquí.
  SumadorRestador_1 : SumadorRestador
    generic map (
      t_d => t_d)
    port map (
      s_r => op_alu(0),                 -- El bit 0 es 0 cuando se suma y 1
                                        -- cuando se resta, con lo cual nos vale
      a   => a,
      b   => b,
      co  => co_sum,
      ov  => ov_sum,
      res => res_sum);

  -- Los multiplicadores y el resto de operadores de la ALU se crean en alto
  -- nivel directamente en este archivo.
  
  res_mul <= std_logic_vector( signed(a) * signed(b) );
  -- No tendremos overflow cuando los bits de la palabra superior del resultado
  -- coincidan con el bit de signo
  ov_mul  <= '0' when (res_mul(2*t_d-1 downto t_d) = c_parte_alta_unos)  and (res_mul(t_d-1) = '1') else
             '0' when (res_mul(2*t_d-1 downto t_d) = c_parte_alta_ceros) and (res_mul(t_d-1) = '0') else
             '1';
  
  res_mulu <= std_logic_vector( unsigned(a) * unsigned(b) );
  -- En este caso no habrá overflow cuando la parte alta del resultado sea todo
  -- ceros.
  ov_mulu  <= '0' when res_mulu(2*t_d-1 downto t_d) = c_parte_alta_ceros else
              '1';

  -- NAND
  res_nand <= not(a and b);

  -- SLL Desplazamiento lógico a izquierda. Se inserta un cero en el bit menos
  -- significativo.
  res_sll <= a(t_d-2 downto 0) & '0';

  -- SRA Desplazamiento arimético a derecha. Se inserta el bit de signo en el
  -- bit más significativo.
  res_sra <= a(t_d-1) & a(t_d-1 downto 1);

  -- SRL Desplazamiento lógico a derecha. Se inserta un cero en el bit más
  -- significativo.
  res_srl <= '0' & a(t_d-1 downto 1);

  -- SLTU 1000 Set on Less Than Unsigned. Pone un 1 en el bit menos
  -- significativo de la salida cuando a < b.
  res_sltu(t_d-1 downto 1) <= (others => '0');
  
  res_sltu(0) <= '1' when unsigned(a) < unsigned(b) else
                 '0';

  -- SLT 1001 Set on Less Than Signed. Igual que el SLTU pero con signo 
  res_slt(0) <= '1' when signed(a) < signed(b) else
  				'0';

  -- Multiplexor de salida para resultado, overflow y acarreo. Cuando alguna de
  -- estas salidas no tiene sentido (por ejemplo en los desplazamientos) se
  -- deja a cero.
  process (res_sum, res_mul, res_mulu, res_nand, res_srl, res_sra,
           res_sltu, res_slt, co_sum, ov_sum, ov_mul, ov_mulu, op_alu)
  begin  -- process
    -- valores por defecto
    res <= (others => '0');
    co  <= '0';
    ov  <= '0';
    case op_alu is
      when c_add =>
        res <= res_sum;
        co  <= co_sum;
        ov  <= ov_sum;
      when c_sub =>
        res <= res_sum;
        co  <= co_sum;
        ov  <= ov_sum;
      when c_mul =>
        res <= res_mul(t_d-1 downto 0);  -- Sólo se usa la parte baja
        ov  <= ov_mul;
      when c_mulu =>
        res <= res_mulu(t_d-1 downto 0);
        ov  <= ov_mulu;
      when c_nand =>
        res <= res_nand;
      when c_sll =>
        res <= res_sll;
      when c_sra =>
        res <= res_sra;
      when c_srl =>
        res <= res_srl;
      when c_sltu =>
        res <= res_sltu;
      when others => null;
    end case;
  end process;

  -- La salida zero se activa cuando a es igual a b
  z <= '1' when a = b else
       '0';
  
end Behavioural;
