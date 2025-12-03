package pl.gamilife.group.controller;

import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.group.controller.request.CreateGroupMemberRequest;
import pl.gamilife.group.controller.request.EditGroupMemberRequest;
import pl.gamilife.group.usecase.creategroupmemberinopengroup.CreateGroupMemberInOpenGroupCommand;
import pl.gamilife.group.usecase.creategroupmemberinopengroup.CreateGroupMemberInOpenGroupResult;
import pl.gamilife.group.usecase.creategroupmemberinopengroup.CreateGroupMemberInOpenGroupUseCase;
import pl.gamilife.group.usecase.editgroupmember.EditGroupMemberCommand;
import pl.gamilife.group.usecase.editgroupmember.EditGroupMemberResult;
import pl.gamilife.group.usecase.editgroupmember.EditGroupMemberUseCase;
import pl.gamilife.group.usecase.leavegroup.LeaveGroupCommand;
import pl.gamilife.group.usecase.leavegroup.LeaveGroupResult;
import pl.gamilife.group.usecase.leavegroup.LeaveGroupUseCase;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/members")
public class GroupMemberController {

    private final CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase;
    private final EditGroupMemberUseCase editGroupMemberUseCase;
    private final LeaveGroupUseCase leaveGroupUseCase;

    @PostMapping
    public ResponseEntity<CreateGroupMemberInOpenGroupResult> save(@RequestBody @Valid CreateGroupMemberRequest request,
                                                                   @PathVariable("groupId") UUID groupId) {
        CreateGroupMemberInOpenGroupResult response = createGroupMemberUseCase.execute(
                new CreateGroupMemberInOpenGroupCommand(groupId, request.userId())
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupMemberId}")
    private ResponseEntity<EditGroupMemberResult> editById(@PathVariable("groupId") UUID groupId,
                                                           @PathVariable("groupMemberId") UUID groupMemberId,
                                                           @RequestBody @Valid EditGroupMemberRequest request) {
        EditGroupMemberResult response = editGroupMemberUseCase.execute(new EditGroupMemberCommand(
                groupId,
                groupMemberId,
                request.groupMoney(),
                request.totalEarnedMoney()
        ));
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{groupMemberId}/leave")
    private ResponseEntity<LeaveGroupResult> leaveGroup(@PathVariable("groupId") UUID groupId,
                                                        @PathVariable("groupMemberId") UUID groupMemberId) {
        LeaveGroupResult response = leaveGroupUseCase.execute(new LeaveGroupCommand(groupId, groupMemberId));
        return ResponseEntity.ok(response);
    }
}
