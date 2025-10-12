package edu.pjwstk.groups.usecase.createchatmessage;

import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/group-members/{groupMemberId}/chat-messages")
public class CreateChatMessageController {

    private final CreateChatMessageUseCase createChatMessageUseCase;

    public CreateChatMessageController(CreateChatMessageUseCase createChatMessageUseCase) {
        this.createChatMessageUseCase = createChatMessageUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateChatMessageResponse> save(@RequestBody @Valid CreateChatMessageRequest request,
                                                          @PathVariable("groupId") UUID groupId,
                                                          @PathVariable("groupMemberId") Integer groupMemberId) {
        CreateChatMessageResponse response = createChatMessageUseCase.execute(request, groupId, groupMemberId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

}
