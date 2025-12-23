package pl.gamilife.task.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.task.TaskApi;
import pl.gamilife.api.task.dto.*;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskCommand;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskResult;
import pl.gamilife.task.application.createtaskforgrouptask.CreateTaskForGroupTaskUseCase;
import pl.gamilife.task.application.deletetask.DeleteTaskCommand;
import pl.gamilife.task.application.deletetask.DeleteTaskUseCase;
import pl.gamilife.task.application.edithabit.EditHabitCommand;
import pl.gamilife.task.application.edithabit.EditHabitUseCase;
import pl.gamilife.task.application.edittask.EditTaskCommand;
import pl.gamilife.task.application.edittask.EditTaskUseCase;
import pl.gamilife.task.application.edittaskforgrouptask.EditTaskForGroupTaskCommand;
import pl.gamilife.task.application.edittaskforgrouptask.EditTaskForGroupTaskResult;
import pl.gamilife.task.application.edittaskforgrouptask.EditTaskForGroupTaskUseCase;
import pl.gamilife.task.application.findhabitbyid.FindHabitByIdCommand;
import pl.gamilife.task.application.findhabitbyid.FindHabitByIdResult;
import pl.gamilife.task.application.findhabitbyid.FindHabitByIdUseCase;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdCommand;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdResult;
import pl.gamilife.task.application.findtaskbyid.FindTaskByIdUseCase;
import pl.gamilife.task.application.getusersactivityitems.GetUsersActivityItemsCommand;
import pl.gamilife.task.application.getusersactivityitems.GetUsersActivityItemsResult;
import pl.gamilife.task.application.getusersactivityitems.GetUsersActivityItemsUseCase;

import java.time.ZoneId;
import java.util.UUID;

@Service
@AllArgsConstructor
public class TaskApiImpl implements TaskApi {

    private final FindTaskByIdUseCase findTaskByIdUseCase;
    private final FindHabitByIdUseCase findHabitByIdUseCase;
    private final DeleteTaskUseCase deleteTaskUseCase;
    private final CreateTaskForGroupTaskUseCase createTaskForGroupTaskUseCase;
    private final EditTaskForGroupTaskUseCase editTaskForGroupTaskUseCase;
    private final GetUsersActivityItemsUseCase getUsersActivityItemsUseCase;
    private final EditTaskUseCase editTaskUseCase;
    private final EditHabitUseCase editHabitUseCase;

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
                request.currentGroupDateTime(),
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
        EditTaskForGroupTaskResult result = editTaskForGroupTaskUseCase.execute(new EditTaskForGroupTaskCommand(
                taskId,
                request.title(),
                request.deadlineDate(),
                request.deadlineTime(),
                request.currentGroupDateTime(),
                request.categoryId(),
                request.difficultyId(),
                request.completed(),
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
    public Page<ActivityItemDto> getAllActivityItemsFiltered(ActivityItemQuery dto) {
        Page<GetUsersActivityItemsResult> result = getUsersActivityItemsUseCase.execute(new GetUsersActivityItemsCommand(
                dto.userId(),
                dto.zoneId(),
                dto.title(),
                dto.categoryId(),
                dto.difficultyId(),
                dto.startDate(),
                dto.endDate(),
                dto.page(),
                dto.size()
        ));

        return result.map(ai -> new ActivityItemDto(
                ai.id(),
                ai.type(),
                ai.title(),
                ai.description(),
                ai.userId(),
                ai.categoryId(),
                ai.categoryName(),
                ai.difficultyId(),
                ai.difficultyName(),
                ai.deadlineDate(),
                ai.deadlineTime(),
                ai.cycleLength(),
                ai.currentStreak(),
                ai.longestStreak(),
                switch (ai.status()) {
                    case ALIVE -> ActivityItemDto.ActivityStatus.ALIVE;
                    case INCOMPLETE -> ActivityItemDto.ActivityStatus.INCOMPLETE;
                    case DEADLINE_TODAY -> ActivityItemDto.ActivityStatus.DEADLINE_TODAY;
                    case DEADLINE_MISSED -> ActivityItemDto.ActivityStatus.DEADLINE_MISSED;
                }
        ));
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
