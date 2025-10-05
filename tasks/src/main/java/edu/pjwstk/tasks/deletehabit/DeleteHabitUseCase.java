package edu.pjwstk.tasks.deletehabit;

import java.util.UUID;

public interface DeleteHabitUseCase {
    void execute(UUID habitId);
}
