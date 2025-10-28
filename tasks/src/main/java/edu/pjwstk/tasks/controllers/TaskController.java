package edu.pjwstk.tasks.controllers;

import edu.pjwstk.tasks.application.createtask.CreateTaskRequest;
import edu.pjwstk.tasks.application.createtask.CreateTaskResponse;
import edu.pjwstk.tasks.application.createtask.CreateTaskUseCase;
import edu.pjwstk.tasks.application.deletetask.DeleteTaskUseCase;
import edu.pjwstk.tasks.application.edittask.EditTaskRequest;
import edu.pjwstk.tasks.application.edittask.EditTaskResponse;
import edu.pjwstk.tasks.application.edittask.EditTaskUseCase;
import edu.pjwstk.tasks.shared.ApiResponse;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks")
public class TaskController {

    private final CreateTaskUseCase createTaskUseCase;
    private final EditTaskUseCase editTaskUseCase;
    private final DeleteTaskUseCase deleteTaskUseCase;

    public TaskController(CreateTaskUseCase createTaskUseCase,
                          EditTaskUseCase editTaskUseCase,
                          DeleteTaskUseCase deleteTaskUseCase) {
        this.createTaskUseCase = createTaskUseCase;
        this.editTaskUseCase = editTaskUseCase;
        this.deleteTaskUseCase = deleteTaskUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateTaskResponse> create(@RequestBody @Valid CreateTaskRequest request) {
        CreateTaskResponse response = createTaskUseCase.execute(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{taskId}")
    public ResponseEntity<EditTaskResponse> edit(@PathVariable UUID taskId,
                                                 @RequestBody @Valid EditTaskRequest request) {
        EditTaskResponse response = editTaskUseCase.execute(request, taskId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{taskId}")
    public ResponseEntity<ApiResponse> delete(@PathVariable UUID taskId) {
        deleteTaskUseCase.execute(taskId);
        return ResponseEntity.ok(new ApiResponse("Task with id: " + taskId + " deleted successfully."));
    }
}
