package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.shared.ApiResponse;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestResponse;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestUseCase;
import edu.pjwstk.groups.usecase.deletegrouprequest.DeleteGroupRequestUseCase;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-requests")
public class GroupRequestController {

    private final CreateGroupRequestUseCase createGroupRequestUseCase;
    private final DeleteGroupRequestUseCase deleteGroupRequestUseCase;

    public GroupRequestController(CreateGroupRequestUseCase createGroupRequestUseCase, DeleteGroupRequestUseCase deleteGroupRequestUseCase) {
        this.createGroupRequestUseCase = createGroupRequestUseCase;
        this.deleteGroupRequestUseCase = deleteGroupRequestUseCase;
    }

    @PostMapping
    private ResponseEntity<CreateGroupRequestResponse> save(@PathVariable("groupId") UUID groupId) {
        CreateGroupRequestResponse response = createGroupRequestUseCase.execute(groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{groupRequestId}")
    private ResponseEntity<ApiResponse> deleteById(@PathVariable("groupRequestId") UUID groupRequestId,
                                                   @PathVariable("groupId") UUID groupId) {
        deleteGroupRequestUseCase.execute(groupRequestId);
        return ResponseEntity.ok(new ApiResponse("Group request with id: " + groupRequestId + " deleted successfully."));
    }
}
