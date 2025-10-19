package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberRequest;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberInOpenGroupUseCase;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberRequest;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberResponse;
import edu.pjwstk.groups.usecase.editgroupmember.EditGroupMemberUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-members")
public class GroupMemberController {

    private final CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase;
    private final EditGroupMemberUseCase editGroupMemberUseCase;

    public GroupMemberController(CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase, EditGroupMemberUseCase editGroupMemberUseCase) {
        this.createGroupMemberUseCase = createGroupMemberUseCase;
        this.editGroupMemberUseCase = editGroupMemberUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateGroupMemberResponse> save(@RequestBody @Valid CreateGroupMemberRequest request,
                                                          @PathVariable("groupId") UUID groupId) {
        CreateGroupMemberResponse response = createGroupMemberUseCase.execute(request, groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @PutMapping("/{groupMemberId}")
    private ResponseEntity<EditGroupMemberResponse> editById(@PathVariable("groupId") String groupId,
                                                             @PathVariable("groupMemberId") UUID groupMemberId,
                                                             @RequestBody @Valid EditGroupMemberRequest request) {
        EditGroupMemberResponse response = editGroupMemberUseCase.execute(groupMemberId, request);
        return ResponseEntity.ok(response);
    }
}
