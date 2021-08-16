library work;
use work.dlx_types.all;
use work.bv_arithmetic.all;

entity alu is
	generic(prop_delay: Time := 5 ns);
	port(operand1, operand2: in dlx_word; operation: in alu_operation_code;
		signed: in bit;
		result: out dlx_word; error: out error_code);
end entity alu;

architecture behaviour of alu is

begin

	alu_behav: process(operand1,operand2,operation,signed) is
		variable resultval: dlx_word;
		variable overflow_flag_set: boolean;
		variable less_than: boolean;

		begin
			error <= "0000";
			case(operation) is
				when "0000" =>
					if signed = '1' then
						bv_add(operand1, operand2, resultval, overflow_flag_set);
						if overflow_flag_set then
							error <= "0001";
						end if;
						result <= resultval after prop_delay;
					else 
						bv_addu(operand1, operand2, resultval, overflow_flag_set);
						if overflow_flag_set then
							error <= "0001";
						end if;
						result <= resultval after prop_delay;
					end if;
				when "0001" =>
					if signed = '1' then
						bv_sub(operand1, operand2, resultval, overflow_flag_set);
						if overflow_flag_set then
							error <= "0001";
						end if;
						result <= resultval after prop_delay;
					else 
						bv_subu(operand1, operand2, resultval, overflow_flag_set);
						if overflow_flag_set then
							error <= "0001";
						end if;
						result <= resultval after prop_delay;
					end if;
				when "0010" =>
					for i in 31 downto 0 loop
						resultval(i) := operand1(i) AND operand2(i);
					end loop;
					result <= resultval after prop_delay;
				when "0011" =>
					for i in 31 downto 0 loop
						resultval(i) := operand1(i) OR operand2(i);
					end loop;
					result <= resultval after prop_delay;
				when "1011" =>
					less_than := bv_lt(operand1, operand2);
					if less_than then
						result <= x"00000001" after prop_delay;
					else
						result <= x"00000000" after prop_delay;
					end if;
				when "1110" =>
					if signed = '1' then
						bv_mult(operand1, operand2, resultval, overflow_flag_set);
						if overflow_flag_set then
							error <= "0001";
						end if;
						result <= resultval after prop_delay;
					else 
						bv_multu(operand1, operand2, resultval, overflow_flag_set);
						if overflow_flag_set then
							error <= "0001";
						end if;
						result <= resultval after prop_delay;
					end if;
				when others =>
					result <= x"00000000" after prop_delay;
			end case;
		end process alu_behav;
end architecture behaviour;
