package edu.pjwstk.tasks.edittask;

import jakarta.validation.Valid;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks")
public class EditTaskController {

    private final EditTaskUseCase editTaskUseCase;

    public EditTaskController(EditTaskUseCase editTaskUseCase) {
        this.editTaskUseCase = editTaskUseCase;
    }

    @PutMapping("/{taskId}")
    public ResponseEntity<EditTaskResponse> editById(@RequestBody @Valid EditTaskRequest request,
                                                     @PathVariable("taskId") UUID taskId) {
        EditTaskResponse response = editTaskUseCase.execute(request, taskId);
        return ResponseEntity.ok(response);
    }

}
