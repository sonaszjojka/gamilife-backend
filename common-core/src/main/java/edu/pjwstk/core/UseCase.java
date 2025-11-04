package edu.pjwstk.core;

public interface UseCase<C extends Command, R> {
    default R execute(C command) {
        command.validate();

        return executeInternal(command);
    }

    R executeInternal(C command);
}
