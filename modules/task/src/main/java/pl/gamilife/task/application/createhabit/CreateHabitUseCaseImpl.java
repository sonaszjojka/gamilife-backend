package pl.gamilife.task.application.createhabit;

import org.springframework.stereotype.Component;
import pl.gamilife.shared.kernel.exception.domain.TaskNotFoundException;
import pl.gamilife.task.entity.Habit;
import pl.gamilife.task.entity.Task;
import pl.gamilife.task.exception.domain.InvalidHabitDataException;
import pl.gamilife.task.repository.HabitRepository;
import pl.gamilife.task.repository.TaskRepository;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
public class CreateHabitUseCaseImpl implements CreateHabitUseCase {

    private final HabitRepository habitRepository;
    private final CreateHabitMapper habitMapper;
    private final TaskRepository taskRepository;

    public CreateHabitUseCaseImpl(HabitRepository habitRepository, CreateHabitMapper habitMapper, TaskRepository taskRepository) {
        this.habitRepository = habitRepository;
        this.habitMapper = habitMapper;
        this.taskRepository = taskRepository;
    }

    @Override
    public CreateHabitResponse execute(CreateHabitRequest request, UUID taskId) {
        if (request.acceptedDate() != null &&
                request.acceptedDate().isBefore(LocalDateTime.now())) {
            throw new InvalidHabitDataException("Accepted date cannot be earlier than creation date");
        }
        Task habitTask = taskRepository.findById(taskId)
                .orElseThrow(() -> new TaskNotFoundException("Task with id " + taskId + " does not exist."));

        if (habitRepository.findHabitByTaskId(taskId).isPresent()) {
            throw new InvalidHabitDataException("Habit for task with id " + taskId + " already exists.");
        }

        Habit habit = habitRepository.save(habitMapper.toEntity(request, UUID.randomUUID(), habitTask));
        return habitMapper.toResponse(habit);
    }
}
