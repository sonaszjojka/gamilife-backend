package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.controller.request.CreateGroupRequest;
import edu.pjwstk.groups.controller.request.EditGroupRequest;
import edu.pjwstk.groups.controller.request.GroupFilterRequest;
import edu.pjwstk.groups.controller.response.ApiResponse;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupCommand;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupResult;
import edu.pjwstk.groups.usecase.creategroup.CreateGroupUseCase;
import edu.pjwstk.groups.usecase.deletegroup.DeleteGroupUseCase;
import edu.pjwstk.groups.usecase.editgroup.EditGroupCommand;
import edu.pjwstk.groups.usecase.editgroup.EditGroupResult;
import edu.pjwstk.groups.usecase.editgroup.EditGroupUseCase;
import edu.pjwstk.groups.usecase.getgroups.GetGroupsCommand;
import edu.pjwstk.groups.usecase.getgroups.GetGroupsResult;
import edu.pjwstk.groups.usecase.getgroups.GetGroupsUseCase;
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
    private final EditGroupUseCase editGroupUseCase;
    private final DeleteGroupUseCase deleteGroupUseCase;
    private final GetGroupsUseCase getGroupsUseCase;

    public GroupController(CreateGroupUseCase createGroupUseCase, EditGroupUseCase editGroupUseCase, DeleteGroupUseCase deleteGroupUseCase, GetGroupsUseCase getGroupsUseCase) {
        this.createGroupUseCase = createGroupUseCase;
        this.editGroupUseCase = editGroupUseCase;
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
    public ResponseEntity<EditGroupResult> save(@RequestBody @Valid EditGroupRequest request,
                                                @PathVariable("groupId") UUID groupId) {
        EditGroupResult response = editGroupUseCase.execute(new EditGroupCommand(
                groupId,
                request.adminId(),
                request.groupName(),
                request.groupCurrencySymbol(),
                request.groupTypeId(),
                request.membersLimit()
        ));
        return ResponseEntity.ok(response);
    }

    @DeleteMapping("/{groupId}")
    public ResponseEntity<ApiResponse> deleteById(@PathVariable("groupId") UUID groupId) {
        deleteGroupUseCase.execute(groupId);
        return ResponseEntity.ok(new ApiResponse("Group with id: " + groupId + " deleted successfully."));
    }

    @GetMapping
    public ResponseEntity<Page<GetGroupsResult>> getAllGroups(
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
        Page<GetGroupsResult> response = getGroupsUseCase.execute(
                new GetGroupsCommand(
                        request.joinCode(),
                        request.type(),
                        request.name(),
                        request.page(),
                        request.size()
                )
        );
        return ResponseEntity.ok(response);
    }
}
