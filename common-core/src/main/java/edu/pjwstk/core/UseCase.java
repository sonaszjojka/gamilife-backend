package edu.pjwstk.core;

public interface UseCase<C extends Command, R> {
    R execute(C command);
}
