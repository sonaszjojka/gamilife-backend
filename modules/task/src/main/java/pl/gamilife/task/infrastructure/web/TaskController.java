package pl.gamilife.task.infrastructure.web;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;
import pl.gamilife.task.application.createtask.CreateTaskCommand;
import pl.gamilife.task.application.createtask.CreateTaskResult;
import pl.gamilife.task.application.createtask.CreateTaskUseCase;
import pl.gamilife.task.application.deletetask.DeleteTaskCommand;
import pl.gamilife.task.application.deletetask.DeleteTaskUseCase;
import pl.gamilife.task.application.edittask.EditTaskCommand;
import pl.gamilife.task.application.edittask.EditTaskResult;
import pl.gamilife.task.application.edittask.EditTaskUseCase;
import pl.gamilife.task.application.getusertasks.GetUserTasksCommand;
import pl.gamilife.task.application.getusertasks.GetUserTasksUseCase;
import pl.gamilife.task.infrastructure.web.request.CreateTaskRequest;
import pl.gamilife.task.infrastructure.web.request.EditTaskRequest;
import pl.gamilife.task.infrastructure.web.response.ApiResponse;
import pl.gamilife.task.infrastructure.web.response.GetUserTasksDto;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/tasks")
public class TaskController {

    private final CreateTaskUseCase createTaskUseCase;
    private final EditTaskUseCase editTaskUseCase;
    private final DeleteTaskUseCase deleteTaskUseCase;
    private final GetUserTasksUseCase getUserTasksUseCase;

    @PostMapping
    public ResponseEntity<CreateTaskResult> create(
            @CurrentUserId UUID userId,
            @RequestBody @Valid CreateTaskRequest request) {
        CreateTaskResult response = createTaskUseCase.execute(new CreateTaskCommand(
                userId,
                request.title(),
                request.description(),
                request.deadlineDate(),
                request.deadlineTime(),
                request.categoryId(),
                request.difficultyId()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PatchMapping("/{taskId}") // TODO: CHANGE TO PATCH
    public ResponseEntity<EditTaskResult> edit(
            @CurrentUserId UUID userId,
            @PathVariable UUID taskId,
            @RequestBody @Valid EditTaskRequest request
    ) {
        EditTaskResult response = editTaskUseCase.execute(new EditTaskCommand(
                userId,
                taskId,
                request.title(),
                request.deadlineDate(),
                request.deadlineTime(),
                request.categoryId(),
                request.difficultyId(),
                request.completed(),
                request.description()
        ));
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{taskId}")
    public ResponseEntity<ApiResponse> delete(@CurrentUserId UUID userId, @PathVariable UUID taskId) {
        deleteTaskUseCase.execute(new DeleteTaskCommand(
                userId, taskId
        ));
        return ResponseEntity.ok(new ApiResponse("Task with id: " + taskId + " deleted successfully."));
    }

    @GetMapping
    public ResponseEntity<Page<GetUserTasksDto>> getUserTasks(
            @CurrentUserId UUID userId,
            @RequestParam(required = false) Integer categoryId,
            @RequestParam(required = false) Integer difficultyId,
            @RequestParam(required = false) Boolean completed,
            @RequestParam(required = false) Boolean isGroupTask,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "10") @Min(1) Integer size
    ) {
        Page<GetUserTasksDto> response = getUserTasksUseCase.execute(new GetUserTasksCommand(
                userId,
                categoryId,
                difficultyId,
                completed,
                isGroupTask,
                page,
                size
        ));
        return ResponseEntity.ok(response);
    }

}
