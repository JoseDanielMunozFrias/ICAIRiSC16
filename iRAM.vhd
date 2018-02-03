library ieee;
use ieee.std_logic_1164.all;
library work;

entity byte_enabled_dual_port_ram is

generic (
ADDR_WIDTH : natural := 6;
MEMORY_WIDTH : natural := 16;
BYTES : natural := 2);
  
port (
we, clk : in  std_logic;
be      : in  std_logic_vector (BYTES - 1 downto 0);
wdata   : in  std_logic_vector(MEMORY_WIDTH - 1 downto 0);
waddr   : in  integer range 0 to 2 ** ADDR_WIDTH -1 ;
raddr   : in  integer range 0 to 2 ** ADDR_WIDTH - 1;
q       : out std_logic_vector(MEMORY_WIDTH-1 downto 0));
end byte_enabled_dual_port_ram;

architecture rtl of byte_enabled_dual_port_ram is
--  build up 2D array to hold the memory
type word_t is array (0 to BYTES-1) of std_logic_vector(7 downto 0);
type ram_t is array (0 to 2 ** ADDR_WIDTH - 1) of word_t;
-- delcare the RAM
signal ram : ram_t;
signal q_local : word_t;

begin  -- rtl
-- Re-organize the read data from the RAM to match the output
--unpack: for i in 0 to BYTES - 1 generate    
--q(BYTE_WIDTH*(i+1) - 1 downto BYTE_WIDTH*i) <= q_local(i);
--end generate unpack;

unpack: for i in 0 to 1 generate
	q(8*(i+1)-1 downto 8*i)<=q_local(i);
	
	end generate unpack;
       
process(clk)
begin
if(rising_edge(clk)) then 
	if(we = '1') then
-- edit this code if using other than four bytes per word
		if(be(0) = '1') then
		ram(waddr)(0) <= wdata(7 downto 0);
		end if;
		if be(1) = '1' then
		ram(waddr)(1) <= wdata(15 downto 8);
		end if;

	end if;
q_local <= ram(raddr);
end if;
end process;  
end rtl;


