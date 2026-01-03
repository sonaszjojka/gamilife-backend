package pl.gamilife.group.controller;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import pl.gamilife.group.controller.request.EditGroupRequestStatusForGroupRequestRequest;
import pl.gamilife.group.controller.response.ApiResponse;
import pl.gamilife.group.usecase.creategrouprequest.CreateGroupRequestCommand;
import pl.gamilife.group.usecase.creategrouprequest.CreateGroupRequestResult;
import pl.gamilife.group.usecase.creategrouprequest.CreateGroupRequestUseCase;
import pl.gamilife.group.usecase.deletegrouprequest.DeleteGroupRequestCommand;
import pl.gamilife.group.usecase.deletegrouprequest.DeleteGroupRequestUseCase;
import pl.gamilife.group.usecase.editgrouprequeststatus.EditGroupRequestStatusCommand;
import pl.gamilife.group.usecase.editgrouprequeststatus.EditGroupRequestStatusResult;
import pl.gamilife.group.usecase.editgrouprequeststatus.EditGroupRequestStatusUseCase;
import pl.gamilife.group.usecase.getgrouprequests.GetGroupRequestsCommand;
import pl.gamilife.group.usecase.getgrouprequests.GetGroupRequestsResult;
import pl.gamilife.group.usecase.getgrouprequests.GetGroupRequestsUseCase;
import pl.gamilife.shared.web.security.annotation.CurrentUserId;

import java.util.UUID;

@RestController
@AllArgsConstructor
@RequestMapping("/api/v1/groups/{groupId}/requests")
public class GroupRequestController {

    private final CreateGroupRequestUseCase createGroupRequestUseCase;
    private final DeleteGroupRequestUseCase deleteGroupRequestUseCase;
    private final EditGroupRequestStatusUseCase editGroupRequestStatusUseCase;
    private final GetGroupRequestsUseCase getGroupRequestsUseCase;


    @PostMapping
    public ResponseEntity<CreateGroupRequestResult> save(
            @CurrentUserId UUID userId,
            @PathVariable UUID groupId
    ) {
        CreateGroupRequestResult response = createGroupRequestUseCase.execute(
                new CreateGroupRequestCommand(userId, groupId)
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    @DeleteMapping("/{groupRequestId}")
    public ResponseEntity<ApiResponse> deleteById(
            @CurrentUserId UUID userId,
            @PathVariable UUID groupRequestId,
            @PathVariable UUID groupId
    ) {
        deleteGroupRequestUseCase.execute(new DeleteGroupRequestCommand(
                userId,
                groupId,
                groupRequestId
        ));
        return ResponseEntity.ok(
                new ApiResponse("Group request with id: " + groupRequestId + " deleted successfully.")
        );
    }

    @PutMapping("/{groupRequestId}/status")
    public ResponseEntity<EditGroupRequestStatusResult> editStatusById(
            @CurrentUserId UUID userId,
            @PathVariable UUID groupRequestId,
            @PathVariable UUID groupId,
            @Valid @RequestBody EditGroupRequestStatusForGroupRequestRequest request
    ) {
        EditGroupRequestStatusResult response = editGroupRequestStatusUseCase.execute(
                new EditGroupRequestStatusCommand(
                        userId,
                        groupId,
                        groupRequestId,
                        request.groupRequestStatusId()
                )
        );
        return ResponseEntity.ok(response);
    }

    @GetMapping
    public ResponseEntity<GetGroupRequestsResult> getAllGroupRequests(
            @PathVariable UUID groupId,
            @RequestParam(required = false) Integer statusId,
            @RequestParam(defaultValue = "0") @Min(0) Integer page,
            @RequestParam(defaultValue = "20") @Min(1) @Max(100) Integer size) {

        GetGroupRequestsResult response = getGroupRequestsUseCase.execute(
                new GetGroupRequestsCommand(
                        groupId,
                        statusId,
                        page,
                        size
                )
        );
        return ResponseEntity.ok(response);
    }

}
