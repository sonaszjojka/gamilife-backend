package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.creategroup.CreateGroupRequest;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupResponse;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupUseCase;
import edu.pjwstk.groups.usecase.updategroup.UpdateGroupRequest;
import edu.pjwstk.groups.usecase.updategroup.UpdateGroupResponse;
import edu.pjwstk.groups.usecase.updategroup.UpdateGroupUseCase;
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

    public GroupController(CreateGroupUseCase createGroupUseCase, UpdateGroupUseCase updateGroupUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.updateGroupUseCase = updateGroupUseCase;
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
}
