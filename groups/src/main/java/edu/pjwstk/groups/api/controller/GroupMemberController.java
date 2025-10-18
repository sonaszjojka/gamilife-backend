package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberRequest;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberResponse;
import edu.pjwstk.groups.usecase.creategroupmember.CreateGroupMemberInOpenGroupUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-members")
public class GroupMemberController {

    private final CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase;

    public GroupMemberController(CreateGroupMemberInOpenGroupUseCase createGroupMemberUseCase) {
        this.createGroupMemberUseCase = createGroupMemberUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateGroupMemberResponse> save(@RequestBody @Valid CreateGroupMemberRequest request,
                                                          @PathVariable("groupId") UUID groupId) {
        CreateGroupMemberResponse response = createGroupMemberUseCase.execute(request, groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
