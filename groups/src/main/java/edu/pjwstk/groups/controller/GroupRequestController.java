package edu.pjwstk.groups.controller;

import edu.pjwstk.groups.controller.request.EditGroupRequestStatusForGroupRequestRequest;
import edu.pjwstk.groups.controller.response.ApiResponse;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestCommand;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestResult;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestUseCase;
import edu.pjwstk.groups.usecase.deletegrouprequest.DeleteGroupRequestCommand;
import edu.pjwstk.groups.usecase.deletegrouprequest.DeleteGroupRequestUseCase;
import edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest.EditGroupRequestStatusForGroupRequestCommand;
import edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest.EditGroupRequestStatusForGroupRequestResult;
import edu.pjwstk.groups.usecase.editgrouprequeststatusforgrouprequest.EditGroupRequestStatusForGroupRequestUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/requests")
public class GroupRequestController {

    private final CreateGroupRequestUseCase createGroupRequestUseCase;
    private final DeleteGroupRequestUseCase deleteGroupRequestUseCase;
    private final EditGroupRequestStatusForGroupRequestUseCase editGroupRequestStatusForGroupRequestUseCase;

    public GroupRequestController(CreateGroupRequestUseCase createGroupRequestUseCase, DeleteGroupRequestUseCase deleteGroupRequestUseCase, EditGroupRequestStatusForGroupRequestUseCase editGroupRequestStatusForGroupRequestUseCase) {
        this.createGroupRequestUseCase = createGroupRequestUseCase;
        this.deleteGroupRequestUseCase = deleteGroupRequestUseCase;
        this.editGroupRequestStatusForGroupRequestUseCase = editGroupRequestStatusForGroupRequestUseCase;
    }

    @PostMapping
    private ResponseEntity<CreateGroupRequestResult> save(@PathVariable("groupId") UUID groupId) {
        CreateGroupRequestResult response = createGroupRequestUseCase.execute(
                new CreateGroupRequestCommand(groupId)
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{groupRequestId}")
    private ResponseEntity<ApiResponse> deleteById(@PathVariable("groupRequestId") UUID groupRequestId,
                                                   @PathVariable("groupId") UUID groupId) {
        deleteGroupRequestUseCase.execute(new DeleteGroupRequestCommand(
                groupId,
                groupRequestId
        ));
        return ResponseEntity.ok(new ApiResponse("Group request with id: " + groupRequestId + " deleted successfully."));
    }

    @PutMapping("/{groupRequestId}/status")
    private ResponseEntity<EditGroupRequestStatusForGroupRequestResult> editStatusById(@PathVariable("groupRequestId") UUID groupRequestId,
                                                                                       @PathVariable("groupId") UUID groupId,
                                                                                       @Valid @RequestBody EditGroupRequestStatusForGroupRequestRequest request) {
        EditGroupRequestStatusForGroupRequestResult response = editGroupRequestStatusForGroupRequestUseCase.execute(
                new EditGroupRequestStatusForGroupRequestCommand(
                        groupId,
                        groupRequestId,
                        request.groupRequestStatusId()
                )
        );
        return ResponseEntity.ok(response);
    }

}
