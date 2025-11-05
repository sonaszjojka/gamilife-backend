package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberInOpenGroupUseCase;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberRequest;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberRequest;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberResponse;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberUseCase;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupResponse;
import edu.pjwstk.groups.usecase.leavegroup.LeaveGroupUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/members")
public class GroupMemberController {

    private final CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase;
    private final EditGroupMemberUseCase editGroupMemberUseCase;
    private final LeaveGroupUseCase leaveGroupUseCase;

    public GroupMemberController(CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase, EditGroupMemberUseCase editGroupMemberUseCase, LeaveGroupUseCase leaveGroupUseCase) {
        this.createGroupMemberUseCase = createGroupMemberUseCase;
        this.editGroupMemberUseCase = editGroupMemberUseCase;
        this.leaveGroupUseCase = leaveGroupUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateGroupMemberResponse> save(@RequestBody @Valid CreateGroupMemberRequest request,
                                                          @PathVariable("groupId") UUID groupId) {
        CreateGroupMemberResponse response = createGroupMemberUseCase.execute(request, groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupMemberId}")
    private ResponseEntity<EditGroupMemberResponse> editById(@PathVariable("groupId") UUID groupId,
                                                             @PathVariable("groupMemberId") UUID groupMemberId,
                                                             @RequestBody @Valid EditGroupMemberRequest request) {
        EditGroupMemberResponse response = editGroupMemberUseCase.execute(groupMemberId, request);
        return ResponseEntity.ok(response);
    }

    @PutMapping("/{groupMemberId}/leave")
    private ResponseEntity<LeaveGroupResponse> leaveGroup(@PathVariable("groupId") UUID groupId,
                                                          @PathVariable("groupMemberId") UUID groupMemberId) {
        LeaveGroupResponse response = leaveGroupUseCase.execute(groupMemberId, groupId);
        return ResponseEntity.ok(response);
    }
}
