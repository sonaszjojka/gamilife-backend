package edu.pjwstk.grouptasks.controllers;

import edu.pjwstk.grouptasks.usecase.creategrouptaskmember.CreateGroupTaskMemberResponse;
import edu.pjwstk.grouptasks.usecase.creategrouptaskmember.CreateGroupTaskMemberUseCase;
import edu.pjwstk.grouptasks.usecase.deletegrouptaskmember.DeleteGroupTaskMemberUseCase;
import edu.pjwstk.grouptasks.usecase.editgrouptaskmember.EditGroupTaskMemberRequest;
import edu.pjwstk.grouptasks.usecase.editgrouptaskmember.EditGroupTaskMemberResponse;
import edu.pjwstk.grouptasks.usecase.editgrouptaskmember.EditGroupTaskMemberUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/group-tasks/")
public class GroupTaskMemberController {
    private final CreateGroupTaskMemberUseCase createGroupTaskMemberUseCase;
    private final DeleteGroupTaskMemberUseCase deleteGroupTaskMemberUseCase;
    private final EditGroupTaskMemberUseCase editGroupTaskMemberUseCase;

    public GroupTaskMemberController(CreateGroupTaskMemberUseCase createGroupTaskMemberUseCase, DeleteGroupTaskMemberUseCase deleteGroupTaskMemberUseCase, EditGroupTaskMemberUseCase editGroupTaskMemberUseCase) {
        this.createGroupTaskMemberUseCase = createGroupTaskMemberUseCase;
        this.deleteGroupTaskMemberUseCase = deleteGroupTaskMemberUseCase;
        this.editGroupTaskMemberUseCase = editGroupTaskMemberUseCase;
    }

    @PostMapping("/{groupTaskId}/group-task-members/{groupMemberId}")
    public ResponseEntity<CreateGroupTaskMemberResponse> save (@PathVariable ("groupTaskId") UUID groupTaskId,
                                                               @PathVariable ("groupMemberId") UUID groupMemberId) {
       CreateGroupTaskMemberResponse response= createGroupTaskMemberUseCase.execute( groupTaskId, groupMemberId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping({"/group-task-members/{groupTaskMemberId}"} )
    public ResponseEntity<Void> delete (@PathVariable ("groupTaskMemberId") UUID groupTaskMemberId)
    {
        deleteGroupTaskMemberUseCase.execute(groupTaskMemberId);
        return ResponseEntity.status(HttpStatus.OK).build();
    }

    @PutMapping({"/group-task-members/{groupTaskMemberId}"})
    public ResponseEntity<EditGroupTaskMemberResponse> update (@PathVariable("groupTaskMemberId") UUID groupTaskMemberId,
                                                               @RequestBody EditGroupTaskMemberRequest req)
    {
        EditGroupTaskMemberResponse response = editGroupTaskMemberUseCase.execute(groupTaskMemberId, req);
        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

}
