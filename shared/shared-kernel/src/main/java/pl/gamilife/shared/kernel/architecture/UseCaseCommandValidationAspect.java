package pl.gamilife.shared.kernel.architecture;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class UseCaseCommandValidationAspect {

    @Before("execution(* pl.gamilife..UseCase+.execute(..)) && args(command)")
    public void validateCommand(Command command) {
        command.validate();
    }

}
