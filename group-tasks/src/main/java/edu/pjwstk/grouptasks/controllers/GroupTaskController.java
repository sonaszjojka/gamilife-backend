package edu.pjwstk.grouptasks.controllers;

import edu.pjwstk.grouptasks.usecase.creategrouptask.CreateGroupTaskRequest;
import edu.pjwstk.grouptasks.usecase.creategrouptask.CreateGroupTaskResponse;
import edu.pjwstk.grouptasks.usecase.creategrouptask.CreateGroupTaskUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/tasks")
public class GroupTaskController {
    private final CreateGroupTaskUseCase createGroupTaskUseCase;
    public GroupTaskController(CreateGroupTaskUseCase createGroupTaskUseCase) {
        this.createGroupTaskUseCase = createGroupTaskUseCase;
    }

    @PostMapping("/{taskId}/group-tasks")
    public ResponseEntity<CreateGroupTaskResponse> save (@PathVariable ("taskId") UUID taskId,
                                                         @RequestBody @Valid CreateGroupTaskRequest request) {
        CreateGroupTaskResponse response= createGroupTaskUseCase.execute(request, taskId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

}
