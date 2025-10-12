package edu.pjwstk.tasks.application.edittasknotification;

import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks/{taskId}/task-notifications")
public class EditTaskNotificationController {

    private final EditTaskNotificationUseCase editTaskNotificationUseCase;

    public EditTaskNotificationController(EditTaskNotificationUseCase editTaskNotificationUseCase) {
        this.editTaskNotificationUseCase = editTaskNotificationUseCase;
    }

    @PutMapping("/{taskNotificationId}")
    public ResponseEntity<EditTaskNotificationResponse> editById(@RequestBody @Valid EditTaskNotificationRequest request,
                                                                 @PathVariable("taskId") UUID taskId,
                                                                 @PathVariable("taskNotificationId") Integer taskNotificationId) {
        EditTaskNotificationResponse response = editTaskNotificationUseCase.execute(request, taskId, taskNotificationId);
        return ResponseEntity.ok(response);
    }

}
