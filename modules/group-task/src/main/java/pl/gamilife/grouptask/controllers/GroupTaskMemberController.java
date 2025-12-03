package pl.gamilife.grouptask.controllers;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.grouptask.shared.ApiResponse;
import pl.gamilife.grouptask.usecase.creategrouptaskmember.CreateGroupTaskMemberRequest;
import pl.gamilife.grouptask.usecase.creategrouptaskmember.CreateGroupTaskMemberResponse;
import pl.gamilife.grouptask.usecase.creategrouptaskmember.CreateGroupTaskMemberUseCase;
import pl.gamilife.grouptask.usecase.deletegrouptaskmember.DeleteGroupTaskMemberUseCase;
import pl.gamilife.grouptask.usecase.editgrouptaskmember.EditGroupTaskMemberRequest;
import pl.gamilife.grouptask.usecase.editgrouptaskmember.EditGroupTaskMemberResponse;
import pl.gamilife.grouptask.usecase.editgrouptaskmember.EditGroupTaskMemberUseCase;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-tasks/{groupTaskId}/participants")
public class GroupTaskMemberController {
    private final CreateGroupTaskMemberUseCase createGroupTaskMemberUseCase;
    private final DeleteGroupTaskMemberUseCase deleteGroupTaskMemberUseCase;
    private final EditGroupTaskMemberUseCase editGroupTaskMemberUseCase;

    public GroupTaskMemberController(CreateGroupTaskMemberUseCase createGroupTaskMemberUseCase, DeleteGroupTaskMemberUseCase deleteGroupTaskMemberUseCase, EditGroupTaskMemberUseCase editGroupTaskMemberUseCase) {
        this.createGroupTaskMemberUseCase = createGroupTaskMemberUseCase;
        this.deleteGroupTaskMemberUseCase = deleteGroupTaskMemberUseCase;
        this.editGroupTaskMemberUseCase = editGroupTaskMemberUseCase;
    }

    @PostMapping()
    public ResponseEntity<CreateGroupTaskMemberResponse> save(@PathVariable("groupTaskId") UUID groupTaskId,
                                                              @RequestBody @Valid CreateGroupTaskMemberRequest request) {

        CreateGroupTaskMemberResponse response = createGroupTaskMemberUseCase.execute(groupTaskId, request);

        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping({"/{participantId}"})
    public ResponseEntity<ApiResponse> delete(@PathVariable("participantId") UUID groupTaskMemberId) {

        deleteGroupTaskMemberUseCase.execute(groupTaskMemberId);

        return ResponseEntity.status(HttpStatus.NO_CONTENT).body(new ApiResponse("Group Task Member" + groupTaskMemberId + "deleted successfully"));
    }

    @PutMapping({"/{participantId}"})
    public ResponseEntity<EditGroupTaskMemberResponse> update(@PathVariable("participantId") UUID groupTaskMemberId,
                                                              @RequestBody @Valid EditGroupTaskMemberRequest req) {
        EditGroupTaskMemberResponse response = editGroupTaskMemberUseCase.execute(groupTaskMemberId, req);

        return ResponseEntity.status(HttpStatus.OK).body(response);
    }

}
