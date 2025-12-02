package pl.gamilife.group.controller;

import pl.gamilife.group.controller.request.CreateGroupRequest;
import pl.gamilife.group.controller.request.EditGroupRequest;
import pl.gamilife.group.controller.request.GroupFilterRequest;
import pl.gamilife.group.controller.response.ApiResponse;
import pl.gamilife.group.usecase.creategroup.CreateGroupCommand;
import pl.gamilife.group.usecase.creategroup.CreateGroupResult;
import pl.gamilife.group.usecase.creategroup.CreateGroupUseCase;
import pl.gamilife.group.usecase.deletegroup.DeleteGroupCommand;
import pl.gamilife.group.usecase.deletegroup.DeleteGroupUseCase;
import pl.gamilife.group.usecase.editgroup.EditGroupCommand;
import pl.gamilife.group.usecase.editgroup.EditGroupResult;
import pl.gamilife.group.usecase.editgroup.EditGroupUseCase;
import pl.gamilife.group.usecase.getgroups.getall.GetGroupsCommand;
import pl.gamilife.group.usecase.getgroups.getall.GetGroupsResult;
import pl.gamilife.group.usecase.getgroups.getall.GetGroupsUseCase;
import pl.gamilife.group.usecase.getgroups.getbyid.GetGroupByIdCommand;
import pl.gamilife.group.usecase.getgroups.getbyid.GetGroupByIdResult;
import pl.gamilife.group.usecase.getgroups.getbyid.GetGroupByIdUseCase;
import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups")
public class GroupController {

    private final CreateGroupUseCase createGroupUseCase;
    private final EditGroupUseCase editGroupUseCase;
    private final DeleteGroupUseCase deleteGroupUseCase;
    private final GetGroupsUseCase getGroupsUseCase;
    private final GetGroupByIdUseCase getGroupByIdUseCase;

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
        deleteGroupUseCase.execute(new DeleteGroupCommand(groupId));
        return ResponseEntity.ok(new ApiResponse("Group with id: " + groupId + " deleted successfully."));
    }

    @GetMapping
    public ResponseEntity<GetGroupsResult> getAllGroups(
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
        GetGroupsResult response = getGroupsUseCase.execute(
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

    @GetMapping("/{groupId}")
    public ResponseEntity<GetGroupByIdResult> getById(
            @PathVariable(name = "groupId") UUID groupId,

            @RequestParam(required = false) Boolean isForLoggedUser
    ) {
        GetGroupByIdResult response = getGroupByIdUseCase.execute(new GetGroupByIdCommand(groupId, isForLoggedUser));
        return ResponseEntity.ok(response);
    }
}
