package pl.gamilife.task.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.HabitDto;
import pl.gamilife.api.task.dto.TaskDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskCommand;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskResult;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskUseCase;
import pl.gamilife.task.application.deletetask.DeleteTaskCommand;
import pl.gamilife.task.application.deletetask.DeleteTaskUseCase;
import pl.gamilife.task.application.edithabit.EditHabitCommand;
import pl.gamilife.task.application.edithabit.EditHabitUseCase;
import pl.gamilife.task.application.edittask.EditTaskCommand;
import pl.gamilife.task.application.edittask.EditTaskResult;
import pl.gamilife.task.application.edittask.EditTaskUseCase;
import pl.gamilife.task.application.findhabitbyid.FindHabitByIdCommand;
import pl.gamilife.task.application.findhabitbyid.FindHabitByIdResult;
import pl.gamilife.task.application.findhabitbyid.FindHabitByIdUseCase;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdCommand;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdResult;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdUseCase;
import pl.gamilife.task.application.findtasksbyids.FindTasksByIdsCommand;
import pl.gamilife.task.application.findtasksbyids.FindTasksByIdsUseCase;

import java.time.ZoneId;
import java.util.Collection;
import java.util.List;
import java.util.UUID;

@Service
@AllArgsConstructor
public class TaskApiImpl implements TaskApi {

    private final FindTaskByIdUseCase findTaskByIdUseCase;
    private final FindHabitByIdUseCase findHabitByIdUseCase;
    private final DeleteTaskUseCase deleteTaskUseCase;
    private final CreateTaskForGroupTaskUseCase createTaskForGroupTaskUseCase;
    private final EditTaskUseCase editTaskUseCase;
    private final EditHabitUseCase editHabitUseCase;
    private final FindTasksByIdsUseCase findTasksByIdsUseCase;

    @Override
    public TaskDto findTaskById(UUID taskId) {
        FindTaskByIdResult result = findTaskByIdUseCase.execute(new FindTaskByIdCommand(taskId));

        return new TaskDto(
                result.id(),
                result.title(),
                result.description(),
                result.userId(),
                new TaskDto.TaskCategoryDto(
                        result.category().id(),
                        result.category().categoryName()
                ),
                new TaskDto.TaskDifficultyDto(
                        result.difficulty().id(),
                        result.difficulty().difficultyName()
                ),
                result.deadlineDate(),
                result.deadlineTime(),
                result.completedAt()
        );
    }

    @Override
    public List<TaskDto> findTasksByIds(Collection<UUID> taskIds) {
        var result = findTasksByIdsUseCase.execute(new FindTasksByIdsCommand(taskIds));

        return result.tasks()
                .stream()
                .map(t -> new TaskDto(
                        t.id(),
                        t.title(),
                        t.description(),
                        t.userId(),
                        new TaskDto.TaskCategoryDto(
                                t.category().id(),
                                t.category().categoryName()
                        ),
                        new TaskDto.TaskDifficultyDto(
                                t.difficulty().id(),
                                t.difficulty().difficultyName()
                        ),
                        t.deadlineDate(),
                        t.deadlineTime(),
                        t.completedAt()
                )).toList();
    }

    @Override
    public HabitDto findHabitById(UUID taskId, UUID userId, ZoneId zoneId) {
        FindHabitByIdResult result = findHabitByIdUseCase.execute(new FindHabitByIdCommand(
                taskId,
                userId,
                zoneId
        ));

        return new HabitDto(
                result.userId(),
                result.canBeWorkedOn()
        );
    }

    @Override
    public void deleteTaskByTaskId(UUID userId, UUID taskId) {
        deleteTaskUseCase.execute(new DeleteTaskCommand(
                userId, taskId
        ));
    }

    @Override
    public TaskForGroupTaskResponseDto createTaskForGroupTask(TaskForGroupTaskRequestDto request) {
        CreateTaskForGroupTaskResult result = createTaskForGroupTaskUseCase.execute(new CreateTaskForGroupTaskCommand(
                request.title(),
                request.deadlineDate(),
                request.deadlineTime(),
                request.currentGroupTimezone(),
                request.categoryId(),
                request.difficultyId(),
                request.description()
        ));

        return new TaskForGroupTaskResponseDto(
                result.taskId(),
                result.title(),
                result.deadlineDate(),
                result.deadlineTime(),
                result.categoryId(),
                result.difficultyId(),
                result.description()
        );
    }

    @Override
    public TaskForGroupTaskResponseDto updateTaskForGroupTask(TaskForGroupTaskRequestDto request, UUID taskId) {
        EditTaskResult result = editTaskUseCase.execute(new EditTaskCommand(
                null,
                request.currentGroupTimezone(),
                taskId,
                request.title(),
                request.removeDescription(),
                request.description(),
                request.deadlineDate(),
                request.removeDeadlineTime(),
                request.deadlineTime(),
                request.categoryId(),
                request.difficultyId(),
                request.completed()
        ));

        return new TaskForGroupTaskResponseDto(
                result.taskId(),
                result.title(),
                result.deadlineDate(),
                result.deadlineTime(),
                result.categoryId(),
                result.difficultyId(),
                result.description()
        );
    }

    @Override
    public void completeTaskById(UUID userId, ZoneId zoneId, UUID taskId) {
        editTaskUseCase.execute(new EditTaskCommand(
                userId,
                zoneId,
                taskId,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                null,
                true
        ));
    }

    @Override
    public void completeHabitById(UUID userId, ZoneId zoneId, UUID habitId) {
        editHabitUseCase.execute(new EditHabitCommand(
                userId,
                zoneId,
                habitId,
                null,
                null,
                null,
                null,
                null,
                null,
                true,
                false
        ));
    }

}
