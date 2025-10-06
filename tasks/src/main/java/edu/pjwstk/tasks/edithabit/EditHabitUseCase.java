package edu.pjwstk.tasks.edithabit;

import jakarta.validation.Valid;
import org.springframework.stereotype.Component;

import java.util.UUID;

@Component
public interface EditHabitUseCase {
    EditHabitResponse execute(EditHabitRequest request, UUID habitId);
}
