package pl.gamilife.shared.kernel.architecture;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import jakarta.validation.Validator;
import lombok.AllArgsConstructor;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.util.Set;

@Aspect
@Order(1)
@Component
@AllArgsConstructor
public class UseCaseCommandValidationAspect {

    private final Validator validator;

    @Around("execution(* pl.gamilife..UseCase+.execute(..)) && args(command)")
    public Object autoValidate(ProceedingJoinPoint pjp, Command command) throws Throwable {
        Set<ConstraintViolation<Object>> violations = validator.validate(command);

        if (!violations.isEmpty()) {
            throw new ConstraintViolationException(violations);
        }

        command.validate();

        return pjp.proceed();
    }

}
