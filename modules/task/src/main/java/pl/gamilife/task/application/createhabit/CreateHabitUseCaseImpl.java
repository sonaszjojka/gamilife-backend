package pl.gamilife.task.application.createhabit;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.domain.exception.domain.InvalidHabitDataException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.HabitRepository;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.web.request.CreateHabitRequest;
import pl.gamilife.task.infrastructure.web.response.CreateHabitResponse;

import java.util.UUID;

@Component
public class CreateHabitUseCaseImpl implements CreateHabitUseCase {

    private final HabitRepository habitRepository;
    private final TaskRepository taskRepository;

    public CreateHabitUseCaseImpl(HabitRepository habitRepository, TaskRepository taskRepository) {
        this.habitRepository = habitRepository;
        this.taskRepository = taskRepository;
    }

    @Override
    public CreateHabitResponse execute(CreateHabitRequest request, UUID taskId) {
        Task habitTask = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + taskId + " does not exist."));

        if (habitRepository.findHabitByTaskId(taskId).isPresent()) {
            throw new InvalidHabitDataException("Habit for task with id " + taskId + " already exists.");
        }

        Habit habit = Habit.builder()
                .taskId(taskId)
                .cycleLength(request.cycleLength())
                .build();
        habit = habitRepository.save(habit);
        return buildResponse(habit);
    }

    private CreateHabitResponse buildResponse(Habit habit) {
        return new CreateHabitResponse(
                habit.getId(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                habit.getCreatedAt(),
                habit.getUpdatedAt()
        );
    }
}
