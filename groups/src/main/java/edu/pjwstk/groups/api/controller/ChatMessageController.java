package edu.pjwstk.groups.api.controller;

import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageRequest;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageResponse;
import edu.pjwstk.groups.usecase.createchatmessage.CreateChatMessageUseCase;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;

@RestController
@RequestMapping("/api/v1/groups/{groupId}/members/{groupMemberId}/chat-messages")
public class ChatMessageController {

    private final CreateChatMessageUseCase createChatMessageUseCase;

    public ChatMessageController(CreateChatMessageUseCase createChatMessageUseCase) {
        this.createChatMessageUseCase = createChatMessageUseCase;
    }

    @PostMapping
    public ResponseEntity<CreateChatMessageResponse> save(@RequestBody @Valid CreateChatMessageRequest request,
                                                          @PathVariable("groupId") UUID groupId,
                                                          @PathVariable("groupMemberId") UUID groupMemberId) {
        CreateChatMessageResponse response = createChatMessageUseCase.execute(request, groupId, groupMemberId);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

}