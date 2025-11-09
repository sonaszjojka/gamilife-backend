package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.shared.ApiResponse;
import edu.pjwstk.groups.controller.request.CreateGroupRequest;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupCommand;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupResult;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupUseCase;
import edu.pjwstk.groups.usecase.deletegroup.DeleteGroupUseCase;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupRequest;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupResponse;
import edu.pjwstk.groups.usecase.editgroup.UpdateGroupUseCase;
import edu.pjwstk.groups.usecase.getgroups.GetGroupsUseCase;
import edu.pjwstk.groups.usecase.getgroups.GroupDto;
import edu.pjwstk.groups.usecase.getgroups.GroupFilterRequest;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import org.springframework.data.domain.Page;
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
    private final GetGroupsUseCase getGroupsUseCase;

    public GroupController(CreateGroupUseCase createGroupUseCase, UpdateGroupUseCase updateGroupUseCase, DeleteGroupUseCase deleteGroupUseCase, GetGroupsUseCase getGroupsUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.updateGroupUseCase = updateGroupUseCase;
        this.deleteGroupUseCase = deleteGroupUseCase;
        this.getGroupsUseCase = getGroupsUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateGroupResult> save(@RequestBody @Valid CreateGroupRequest request) {
        CreateGroupResult response = createGroupUseCase.execute(new CreateGroupCommand(
                request.groupName(),
                request.groupCurrencySymbol(),
                request.groupTypeId(),
                request.membersLimit()
        ));
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

    @GetMapping
    public ResponseEntity<Page<GroupDto>> getAllGroups(
            @RequestParam(required = false) String joinCode,

            @RequestParam(required = false) Integer groupType,

            @RequestParam(required = false) String groupName,

            @RequestParam(defaultValue = "0")
            @Min(0) Integer page,

            @RequestParam(defaultValue = "10")
            @Min(1) @Max(100) Integer size
    ) {
        GroupFilterRequest request = new GroupFilterRequest(
                joinCode, groupType, groupName, page, size
        );
        Page<GroupDto> response = getGroupsUseCase.execute(request);
        return ResponseEntity.ok(response);
    }
}
