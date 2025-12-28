package pl.gamilife.shared.kernel.architecture;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import jakarta.validation.ValidationException;
import jakarta.validation.Validator;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

import java.util.Set;

@Slf4j
@Aspect
@Order(1)
@Component
@AllArgsConstructor
public class UseCaseCommandValidationAspect {

    private final Validator validator;

    @Around("execution(* pl.gamilife..UseCase+.execute(..)) && args(command)")
    public Object autoValidate(ProceedingJoinPoint pjp, Command command) throws Throwable {
        String commandName = command.getClass().getSimpleName();
        log.info("Processing {}", commandName);
        Set<ConstraintViolation<Object>> violations = validator.validate(command);

        if (!violations.isEmpty()) {
            log.error("Validation error(s) for {}. Violations: {}", commandName, violations);
            throw new ConstraintViolationException(violations);
        }

        try {
            command.validate();
        } catch (Exception e) {
            log.error("Complex validation error(s) for {}. Violations: {}", commandName, e.getMessage());
            throw new ValidationException(e.getMessage());
        }

        return pjp.proceed();
    }

}
