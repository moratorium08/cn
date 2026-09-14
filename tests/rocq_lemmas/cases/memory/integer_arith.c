/* CN integer arithmetic must be exported with the semantics lib/solver.ml
 * gives it: SMT-LIB Euclidean div/mod, Z3 rem, shifts as scaling by 2^n. */
/*@
lemma euclidean_division(integer a, integer b)
  requires b != 0;
  ensures a == b * (a / b) + mod(a, b);
    0 <= mod(a, b); mod(a, b) < abs(b);

lemma negative_divisor_division()
  requires true;
  ensures (-7) / (-2) == 4; mod(-7, -2) == 1; rem(-7, -2) == -1;
    (-7) / 2 == -4; 7 / (-2) == -3;
    mod(-7, 2) == 1; rem(-7, 2) == 1;

lemma remainder_sign_follows_divisor(integer a, integer b)
  requires b < 0;
  ensures rem(a, b) == -mod(a, b);

lemma shift_scaling(integer x, integer n)
  requires 0 <= n;
  ensures shift_left(x, n) == x * power(2, n);
    shift_right(x, n) == x / power(2, n);

lemma negation_cancels(integer x)
  requires true;
  ensures x + (-x) == 0; abs(-x) == abs(x);
@*/
