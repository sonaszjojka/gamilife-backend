package pl.gamilife.task.application.findtasksbyids;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.port.repository.TaskRepository;

import java.util.List;

@Service
@AllArgsConstructor
public class FindTasksByIdsUseCaseImpl implements FindTasksByIdsUseCase {

    private final TaskRepository taskRepository;

    @Override
    public FindTasksByIdsResult execute(FindTasksByIdsCommand cmd) {
        List<Task> tasks = taskRepository.findAllByIdIn(cmd.taskIds());
        return buildResponse(tasks);
    }

    private FindTasksByIdsResult buildResponse(List<Task> tasks) {
        return new FindTasksByIdsResult(
                tasks.stream().map(task -> new FindTasksByIdsResult.TaskDto(
                                task.getId(),
                                task.getTitle(),
                                task.getDescription(),
                                task.getUserId(),
                                new FindTasksByIdsResult.TaskCategoryDto(task.getCategoryId(), task.getCategory().getName()),
                                new FindTasksByIdsResult.TaskDifficultyDto(task.getDifficultyId(), task.getDifficulty().getName()),
                                task.getDeadlineDate(),
                                task.getDeadlineTime(),
                                task.getCompletedAt()
                        )
                ).toList()
        );
    }
}
