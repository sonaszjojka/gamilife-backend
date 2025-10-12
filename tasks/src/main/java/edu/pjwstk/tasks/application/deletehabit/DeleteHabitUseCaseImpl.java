package edu.pjwstk.tasks.application.deletehabit;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.exception.HabitNotFoundException;
import edu.pjwstk.tasks.repository.HabitRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.UUID;

@Component
public class DeleteHabitUseCaseImpl implements DeleteHabitUseCase {
    private final HabitRepository habitRepository;

    public DeleteHabitUseCaseImpl(HabitRepository habitRepository) {
        this.habitRepository = habitRepository;
    }

    @Override
    @Transactional
    public void execute(UUID habitId) {
        Habit habit = habitRepository
                .findById(habitId)
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit with id " + habitId + " not found!"
                ));
        
        habitRepository.deleteById(habitId);
    }
}
