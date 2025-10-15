package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestResponse;
import edu.pjwstk.groups.usecase.creategrouprequest.CreateGroupRequestUseCase;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-requests")
public class GroupRequestController {

    private final CreateGroupRequestUseCase createGroupRequestUseCase;

    public GroupRequestController(CreateGroupRequestUseCase createGroupRequestUseCase) {
        this.createGroupRequestUseCase = createGroupRequestUseCase;
    }

    @PostMapping
    private ResponseEntity<CreateGroupRequestResponse> save(@PathVariable("groupId") UUID groupId) {
        CreateGroupRequestResponse response = createGroupRequestUseCase.execute(groupId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }
}
