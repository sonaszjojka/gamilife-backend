package edu.pjwstk.tasks.application.deletehabit;

import java.util.UUID;

public interface DeleteHabitUseCase {
    void execute(UUID habitId);
}
