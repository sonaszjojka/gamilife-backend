package pl.gamilife.task.application.deletehabit;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.task.domain.exception.domain.HabitNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.port.repository.HabitRepository;

@Component
@AllArgsConstructor
public class DeleteHabitUseCaseImpl implements DeleteHabitUseCase {

    private final HabitRepository habitRepository;

    @Override
    @Transactional
    public Void execute(DeleteHabitCommand cmd) {
        Habit habit = habitRepository
                .findById(cmd.habitId())
                .orElseThrow(() -> new HabitNotFoundException(String.format(
                        "Habit with id %s not found!",
                        cmd.habitId()
                )));
        habitRepository.delete(habit);

        return null;
    }
}
