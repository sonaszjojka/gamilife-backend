package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.controller.request.CreateGroupMemberRequest;
import edu.pjwstk.groups.controller.request.EditGroupMemberRequest;
import edu.pjwstk.groups.usecase.creategroupmemberinopengroup.CreateGroupMemberInOpenGroupCommand;
import edu.pjwstk.groups.usecase.creategroupmemberinopengroup.CreateGroupMemberInOpenGroupResult;
import edu.pjwstk.groups.usecase.creategroupmemberinopengroup.CreateGroupMemberInOpenGroupUseCase;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberCommand;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberResult;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberUseCase;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupResult;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupUseCase;
import jakarta.validation.Valid;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

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
                new CreateGroupMemberInOpenGroupCommand(request.userId(), groupId)
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
        LeaveGroupResult response = leaveGroupUseCase.execute(groupMemberId, groupId);
        return ResponseEntity.ok(response);
    }
}
