package edu.pjwstk.grouptasks.controllers;

import edu.pjwstk.grouptasks.shared.ApiResponse;
import edu.pjwstk.grouptasks.usecase.creategrouptask.CreateGroupTaskRequest;
import edu.pjwstk.grouptasks.usecase.creategrouptask.CreateGroupTaskResponse;
import edu.pjwstk.grouptasks.usecase.creategrouptask.CreateGroupTaskUseCase;
import edu.pjwstk.grouptasks.usecase.deletegrouptask.DeleteGroupTaskUseCase;
import edu.pjwstk.grouptasks.usecase.editgrouptask.EditGroupTaskRequest;
import edu.pjwstk.grouptasks.usecase.editgrouptask.EditGroupTaskResponse;
import edu.pjwstk.grouptasks.usecase.editgrouptask.EditGroupTaskUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-tasks")
public class GroupTaskController {
    private final CreateGroupTaskUseCase createGroupTaskUseCase;
    private final DeleteGroupTaskUseCase deleteGroupTaskUseCase;
    private final EditGroupTaskUseCase editGroupTaskUseCase;
    public GroupTaskController(CreateGroupTaskUseCase createGroupTaskUseCase, DeleteGroupTaskUseCase deleteGroupTaskUseCase, EditGroupTaskUseCase editGroupTaskUseCase) {
        this.createGroupTaskUseCase = createGroupTaskUseCase;
        this.deleteGroupTaskUseCase = deleteGroupTaskUseCase;
        this.editGroupTaskUseCase = editGroupTaskUseCase;
    }

    @PostMapping()
    public ResponseEntity<CreateGroupTaskResponse> save (@PathVariable ("groupId") UUID taskId,
                                                         @RequestBody @Valid CreateGroupTaskRequest request) {
        CreateGroupTaskResponse response= createGroupTaskUseCase.execute(request, taskId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{groupTaskId}")
    public ResponseEntity<ApiResponse> delete (@PathVariable ("groupTaskId") UUID groupTaskId) {
        deleteGroupTaskUseCase.execute(groupTaskId);
        return ResponseEntity.status(HttpStatus.OK).body(new ApiResponse("Group Task with id: " + groupTaskId + " deleted successfully"));
    }

    @PutMapping("/{groupTaskId}")
    public ResponseEntity<EditGroupTaskResponse> edit (@PathVariable ("groupTaskId") UUID groupTaskId,
                                                       @RequestBody @Valid EditGroupTaskRequest request) {
        EditGroupTaskResponse response = editGroupTaskUseCase.execute(groupTaskId, request);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

}
