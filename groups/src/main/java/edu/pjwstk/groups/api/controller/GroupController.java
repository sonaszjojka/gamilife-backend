package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.shared.ApiResponse;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupRequest;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupResponse;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupUseCase;
import edu.pjwstk.groups.usecase.deletegroup.DeleteGroupUseCase;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupRequest;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupResponse;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups")
public class GroupController {

    private final CreateGroupUseCase createGroupUseCase;
    private final UpdateGroupUseCase updateGroupUseCase;
    private final DeleteGroupUseCase deleteGroupUseCase;

    public GroupController(CreateGroupUseCase createGroupUseCase, UpdateGroupUseCase updateGroupUseCase, DeleteGroupUseCase deleteGroupUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.updateGroupUseCase = updateGroupUseCase;
        this.deleteGroupUseCase = deleteGroupUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateGroupResponse> save(@RequestBody @Valid CreateGroupRequest request) {
        CreateGroupResponse response = createGroupUseCase.execute(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupId}")
    public ResponseEntity<UpdateGroupResponse> save(@RequestBody @Valid UpdateGroupRequest request,
                                                    @PathVariable("groupId") UUID groupId) {
        UpdateGroupResponse response = updateGroupUseCase.execute(request, groupId);
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{groupId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("groupId") UUID groupId) {
        deleteGroupUseCase.execute(groupId);
        return ResponseEntity.ok(new ApiResponse("Group with id: " + groupId + " deleted successfully."));
    }
}
