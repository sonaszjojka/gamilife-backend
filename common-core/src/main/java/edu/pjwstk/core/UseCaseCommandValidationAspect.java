package edu.pjwstk.core;

import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Before;
import org.springframework.stereotype.Component;

@Aspect
@Component
public class UseCaseCommandValidationAspect {

    @Before("execution(* edu.pjwstk..UseCase+.execute(..)) && args(command)")
    public void validateCommand(Command command) {
        command.validate();
    }

}
