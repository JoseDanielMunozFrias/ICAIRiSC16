-- Implantación Multiciclo de la arquitectura ICAI-RiSC-16
-- Unidad de control.
--
-- Versión 1.0
-- Autor: José Daniel Muñoz Frías. ICAIdea.

library ieee;
use ieee.std_logic_1164.all;
use work.ConstantesICAIRiSC16.all;

entity Control is
  
  port (
    clk       : in  std_logic;
    reset_n   : in  std_logic;
    cod_op    : in  std_logic_vector(3 downto 0); -- In the new version it is a 3
    cod_fun   : in  std_logic_vector(3 downto 0);
    z         : in  std_logic;          -- Flag Zero (para BEQ)
    en_pc     : out std_logic;          -- Escribe el PC
    en_ir     : out std_logic;          -- enable del IR
    en_banco  : out std_logic;          -- enable del banco de registros
    en_r_alu  : out std_logic;          -- en del reg de salida de la ALU
    r_w       : out std_logic;          -- escritura (0 OJO) en la RAM
    sel_pc    : out std_logic_vector(1 downto 0);   -- Mux de entrada al PC
    sel_dir_e : out std_logic_vector(1 downto 0);   -- Nº reg escritura
    sel_dat_e : out std_logic_vector(1 downto 0);   -- Dato de entrada al banco
    sel_alu_a : out std_logic;          -- Mux de entrada A a la ALU
    sel_alu_b : out std_logic_vector(1 downto 0);   -- Mux entrada B ALU
    op_alu    : out std_logic_vector(3 downto 0));  -- Operación de la ALU

end Control;

architecture Behavioural of Control is
  type t_estados is (Reset, Fetch, Decod, Lui3, Beq3, Jalr3, Arit3, Arit4,
                     Addi3, Addi4, Lwsw3, Lw4, Lw5, Sw4, InstDesc3);
  signal estado_act, estado_sig : t_estados;

  signal wr_pc, wr_pc_cond : std_logic;  -- Para generar en_pc
  
begin  -- Behavioural

  -- Lógica de generación del enable para el PC en función de si es una
  -- instrucción normal o salto incondicional o es un salto condicional
  en_pc <= wr_pc or (z and wr_pc_cond);


  VarEstado : process (clk, reset_n)
  begin
    if reset_n = '0' then  -- Reset as--@@--ncrono
      estado_act <= Reset;              -- (activo bajo)
    elsif clk'event and clk = '1' then
      estado_act <= estado_sig;
    end if;
  end process VarEstado;

  TransicionEstados : process (estado_act, cod_op)
  begin
    -- Por defecto nos quedamos en el estado actual
    estado_sig <= estado_act;
    case estado_act is
      when Reset =>
        estado_sig <= Fetch;
      when Fetch =>
        estado_sig <= Decod;
      when Decod =>
        case cod_op is
          when c_lui =>
            estado_sig <= Lui3;
          when c_arit =>
            estado_sig <= Arit3;
          when c_beq =>
            estado_sig <= Beq3;
          when c_addi =>
            estado_sig <= Addi3; -- Tengo que añadir más posibilidades para las nuevas instrucciones
          when c_lb =>
            estado_sig <= Lwsw3;
          when c_sb =>
            estado sig <= Lwsw3;

          when c_lw =>
            estado_sig <= Lwsw3; 
          when c_sw =>
            estado_sig <= Lwsw3;        -- Lw y sw comparten estado 3.
          when c_jalr =>
            estado_sig <= Jalr3;
          when others =>
            estado_sig <= InstDesc3;    -- Estado para parar la CPU si llega
                                        -- una instrucción desconocida. En
                                        -- futuras versiones se podría generar
                                        -- una excepción
        end case;
      when Lui3 =>
        estado_sig <= Fetch;
      when Arit3 =>
        estado_sig <= Arit4;
      when Arit4 =>
        estado_sig <= Fetch;
      when Beq3 =>
        estado_sig <= Fetch;
      when Addi3 =>
        estado_sig <= Addi4;
      when Addi4 =>
        estado_sig <= Fetch;
      when Lwsw3 =>
        if cod_op = c_lw then
          estado_sig <= Lw4;
        if cod_op = c_lb then
          estado_sig <= Lw4;
        if cod_op = c_sw then
          estado_sig <= Sw4;
        elsif cod_op = c_sb then
          estado_sig <= Sw4;  
        else
          estado_sig <= InstDesc3;      -- No se debería llegar aquí nunca
        end if;
      when Lw4 =>
        estado_sig <= Lw5;              -- He modificado los nombres al mínimo para mantener la nomenclatura similar. Aun así, tal vez las estapas 4 y 5 simplemente podrían llamarse load4 y store4 y sería más claro
      when Lw5 =>
        estado_sig <= Fetch;
      when SW4 =>
        estado_sig <= Fetch;
      when Jalr3 =>
        estado_sig <= Fetch;
      when InstDesc3 =>
        estado_sig <= InstDesc3;        -- Nos quedamos parados para detectar
                                        -- el error.
      when others =>
        estado_sig <= Fetch;
    end case;
  end process TransicionEstados;

  Salidas : process (estado_act)
  begin  -- process Salidas
    -- Valores por defecto de todas las salidas de la máquina
    wr_pc      <= '0';
    wr_pc_cond <= '0';
    en_ir      <= '0';
    en_banco   <= '0';
    en_r_alu   <= '0';
    r_w        <= '1';  -- Ojo cuando vale 1 se lee (inofensivo) y cuando vale 0 se
    -- escribe en el flanco de reloj
    sel_pc     <= "--";                 -- Para que simplifique más se dejan
    -- los MUX a don't care.
    sel_dir_e  <= "--";
    sel_dat_e  <= "--";
    sel_alu_a  <= '-';
    sel_alu_b  <= "--";
    op_alu     <= "----";

    case estado_act is
      when Reset =>
        null;                           -- Si durante el reset entramos en
                                        -- Fetch, el registro de direcciones de
                                        -- la ROM se carga con 1 y por tanto no
                                        -- se ejecuta la instrucción 0.
        sel_pc <= "00";
        wr_pc  <= '1';                  -- Pongo a 0000 en PC.
      when Fetch =>
        -- PC = PC+1
        wr_pc     <= '1';
        sel_pc    <= "01";
        sel_alu_a <= '1';
        sel_alu_b <= "01";
        op_alu    <= c_add;
        -- IR = MemInst(PC)
        en_ir     <= '1';
      when Decod =>
        -- r_alu = PC + signExt(Inmediato)
        sel_alu_a <= '1';
        sel_alu_b <= "10";
        op_alu    <= c_add;
        en_r_alu  <= '1';
      when Lui3 =>
        -- ParteAlta de Banco(Rs) = Inmediato 10 (IR(9 downto 0)
        sel_dat_e <= "11";
        sel_dir_e <= "10";
        en_banco  <= '1';
      when Arit3 =>
        -- R_ALU = RegA op RegB 
        sel_alu_a <= '0';
        sel_alu_b <= "00";
        op_alu    <= cod_fun;
        en_r_alu  <= '1';               -- OJO. Probar si se puede escribir
                                        -- directamente en el banco sin
                                        -- aumentar el periodo de reloj
      when Arit4 =>
        -- Banco(Rd) = R_ALU
        sel_dir_e <= "00";
        sel_dat_e <= "00";
        en_banco  <= '1';
      when Beq3 =>
        -- Resta registros y salto si cero
        sel_alu_a  <= '0';              -- Resta de los dos registros
        sel_alu_b  <= "00";
        op_alu     <= c_sub;
        sel_pc     <= "00";             -- PC se carga con R_ALU si 
        wr_pc_cond <= '1';              -- ha lugar (z = 1)
      when Addi3 =>
        sel_alu_a <= '0';
        sel_alu_b <= "10";
        op_alu    <= c_add;
        en_r_alu  <= '1';
      when Addi4 =>
        sel_dat_e <= "00";
        sel_dir_e <= "01";
        en_banco  <= '1';
      when Lwsw3 =>
        sel_alu_a <= '0';
        sel_alu_b <= "10";
        op_alu    <= c_add;
        en_r_alu  <= '1';
      when Lw4 =>
        null;                -- just wait the RAM for reading the data
      when LW5 =>
        sel_dat_e <= "01";
        sel_dir_e <= "01";
        en_banco  <= '1';
      when Sw4 =>
        r_w <= '0';
      when Jalr3 =>
        sel_dat_e <= "10";
        sel_dir_e <= "01";
        en_banco  <= '1';
        sel_pc    <= "10";
        wr_pc     <= '1';
      when others => null;
    end case;
  end process Salidas;
  
  

end Behavioural;
