package pl.gamilife.task.infrastructure.web;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;
import pl.gamilife.task.application.createtasknotification.CreateTaskNotificationCommand;
import pl.gamilife.task.application.createtasknotification.CreateTaskNotificationResult;
import pl.gamilife.task.application.createtasknotification.CreateTaskNotificationUseCase;
import pl.gamilife.task.application.deletetasknotification.DeleteTaskNotificationCommand;
import pl.gamilife.task.application.deletetasknotification.DeleteTaskNotificationUseCase;
import pl.gamilife.task.application.edittasknotification.EditTaskNotificationCommand;
import pl.gamilife.task.application.edittasknotification.EditTaskNotificationResponse;
import pl.gamilife.task.application.edittasknotification.EditTaskNotificationUseCase;
import pl.gamilife.task.infrastructure.web.request.CreateTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.request.EditTaskNotificationRequest;
import pl.gamilife.task.infrastructure.web.response.ApiResponse;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/{taskId}/task-notifications")
public class TaskNotificationController {

    private final CreateTaskNotificationUseCase createTaskNotificationUseCase;
    private final EditTaskNotificationUseCase editTaskNotificationUseCase;
    private final DeleteTaskNotificationUseCase deleteTaskNotificationUseCase;

    public TaskNotificationController(CreateTaskNotificationUseCase createTaskNotificationUseCase,
                                      EditTaskNotificationUseCase editTaskNotificationUseCase,
                                      DeleteTaskNotificationUseCase deleteTaskNotificationUseCase) {
        this.createTaskNotificationUseCase = createTaskNotificationUseCase;
        this.editTaskNotificationUseCase = editTaskNotificationUseCase;
        this.deleteTaskNotificationUseCase = deleteTaskNotificationUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateTaskNotificationResult> create(
            @CurrentUserId UUID userId,
            @PathVariable UUID taskId,
            @RequestBody @Valid CreateTaskNotificationRequest request
    ) {
        CreateTaskNotificationResult response = createTaskNotificationUseCase.execute(new CreateTaskNotificationCommand(
                userId,
                taskId,
                request.sendDate()
        ));
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PatchMapping("/{taskNotificationId}") // TODO: CHANGE TO PATCH
    public ResponseEntity<EditTaskNotificationResponse> edit(
            @CurrentUserId UUID userId,
            @PathVariable UUID taskId,
            @PathVariable UUID taskNotificationId,
            @RequestBody @Valid EditTaskNotificationRequest request
    ) {
        EditTaskNotificationResponse response =
                editTaskNotificationUseCase.execute(new EditTaskNotificationCommand(
                        userId,
                        taskId,
                        taskNotificationId,
                        request.sendDate()
                ));
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{taskNotificationId}")
    public ResponseEntity<ApiResponse> delete(
            @CurrentUserId UUID userId,
            @PathVariable UUID taskId,
            @PathVariable UUID taskNotificationId
    ) {
        deleteTaskNotificationUseCase.execute(new DeleteTaskNotificationCommand(
                userId,
                taskId,
                taskNotificationId
        ));
        return ResponseEntity.ok(
                new ApiResponse("Task notification with id " + taskNotificationId + " deleted successfully.")
        );
    }
}
