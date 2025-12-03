package pl.gamilife.task.application.deletehabit;

import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.exception.domain.HabitNotFoundException;
import pl.gamilife.task.repository.HabitRepository;

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
        habitRepository
                .findById(habitId)
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit with id " + habitId + " not found!"
                ));

        habitRepository.deleteById(habitId);
    }
}
