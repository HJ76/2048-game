Public Class Form1
    Dim r As Random = New Random
    Dim a(,) As Integer = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}
    Dim b(,) As Integer = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}
    Dim old(,) As Integer = {{0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}
    Dim score As Integer = 0
    Dim spot As Integer
    Dim temp As Integer

    Private Sub updatearray()
        If a(0, 0) = 0 Then
            Label1.Text = ""
        Else
            Label1.Text = a(0, 0)
        End If
        If a(0, 1) = 0 Then
            Label2.Text = ""
        Else
            Label2.Text = a(0, 1)
        End If
        If a(0, 2) = 0 Then
            Label3.Text = ""
        Else
            Label3.Text = a(0, 2)
        End If
        If a(0, 3) = 0 Then
            Label4.Text = ""
        Else
            Label4.Text = a(0, 3)
        End If
        If a(1, 0) = 0 Then
            Label5.Text = ""
        Else
            Label5.Text = a(1, 0)
        End If
        If a(1, 1) = 0 Then
            Label6.Text = ""
        Else
            Label6.Text = a(1, 1)
        End If
        If a(1, 2) = 0 Then
            Label7.Text = ""
        Else
            Label7.Text = a(1, 2)
        End If
        If a(1, 3) = 0 Then
            Label8.Text = ""
        Else
            Label8.Text = a(1, 3)
        End If
        If a(2, 0) = 0 Then
            Label9.Text = ""
        Else
            Label9.Text = a(2, 0)
        End If
        If a(2, 1) = 0 Then
            Label10.Text = ""
        Else
            Label10.Text = a(2, 1)
        End If
        If a(2, 2) = 0 Then
            Label11.Text = ""
        Else
            Label11.Text = a(2, 2)
        End If
        If a(2, 3) = 0 Then
            Label12.Text = ""
        Else
            Label12.Text = a(2, 3)
        End If
        If a(3, 0) = 0 Then
            Label13.Text = ""
        Else
            Label13.Text = a(3, 0)
        End If
        If a(3, 1) = 0 Then
            Label14.Text = ""
        Else
            Label14.Text = a(3, 1)
        End If
        If a(3, 2) = 0 Then
            Label15.Text = ""
        Else
            Label15.Text = a(3, 2)
        End If
        If a(3, 3) = 0 Then
            Label16.Text = ""
        Else
            Label16.Text = a(3, 3)
        End If
    End Sub
    Private Sub left1()
        Dim i As Integer
        Dim j As Integer
        Dim k As Integer
        For i = 0 To 3
            For j = 0 To 2
                k = j
                Do
                    k = k + 1
                Loop Until a(i, k) <> 0 Or k = 3
                If a(i, j) = a(i, k) Then
                    a(i, k) = 0
                    a(i, j) = 2 * a(i, j)
                    score += a(i, j)
                End If
            Next
            For j = 0 To 2
                If a(i, j) = 0 Then
                    k = j
                    Do
                        k = k + 1
                    Loop Until a(i, k) <> 0 Or k = 3
                    If a(i, k) <> 0 Then
                        a(i, j) = a(i, k)
                        a(i, k) = 0
                    End If
                End If
            Next
        Next
    End Sub
    Private Sub right1()
        Dim i As Integer
        Dim j As Integer
        Dim k As Integer
        For i = 0 To 3
            For j = 3 To 1 Step -1
                k = j
                Do
                    k = k - 1
                Loop Until a(i, k) <> 0 Or k = 0
                If a(i, j) = a(i, k) Then
                    a(i, k) = 0
                    a(i, j) = 2 * a(i, j)
                    score += a(i, j)
                End If
            Next
            For j = 3 To 1 Step -1
                If a(i, j) = 0 Then
                    k = j
                    Do
                        k = k - 1
                    Loop Until a(i, k) <> 0 Or k = 0
                    If a(i, k) <> 0 Then
                        a(i, j) = a(i, k)
                        a(i, k) = 0
                    End If
                End If
            Next
        Next
    End Sub
    Private Sub up()
        Dim i2 As Integer
        Dim j2 As Integer
        Dim k2 As Integer
        For i2 = 0 To 3
            For j2 = 0 To 2
                k2 = j2
                Do
                    k2 = k2 + 1
                Loop Until a(k2, i2) <> 0 Or k2 = 3
                If a(j2, i2) = a(k2, i2) Then
                    a(k2, i2) = 0
                    a(j2, i2) = 2 * a(j2, i2)
                    score += a(j2, i2)
                End If
            Next
            For j2 = 0 To 2
                If a(j2, i2) = 0 Then
                    k2 = j2
                    Do
                        k2 = k2 + 1
                    Loop Until a(k2, i2) <> 0 Or k2 = 3
                    If a(k2, i2) <> 0 Then
                        a(j2, i2) = a(k2, i2)
                        a(k2, i2) = 0
                    End If
                End If
            Next
        Next
    End Sub
    Private Sub down()
        Dim i As Integer
        Dim j As Integer
        Dim k As Integer
        For i = 0 To 3
            For j = 3 To 1 Step -1
                k = j
                Do
                    k = k - 1
                Loop Until a(k, i) <> 0 Or k = 0
                If a(j, i) = a(k, i) Then
                    a(k, i) = 0
                    a(j, i) = 2 * a(j, i)
                    score += a(j, i)
                End If
            Next
            For j = 3 To 1 Step -1
                If a(j, i) = 0 Then
                    k = j
                    Do
                        k = k - 1
                    Loop Until a(k, i) <> 0 Or k = 0
                    If a(k, i) <> 0 Then
                        a(j, i) = a(k, i)
                        a(k, i) = 0
                    End If
                End If
            Next
        Next
    End Sub
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim i As Integer = r.Next(0, 4)
        Dim j As Integer = r.Next(0, 4)
        Dim k As Integer = r.Next(0, 1)
        If k = 0 Then
            a(i, j) = 2
        ElseIf k = 1 Then
            a(i, j) = 4
        End If
        Dim i1 As Integer = r.Next(0, 4)
        Dim j1 As Integer = r.Next(0, 4)
        Dim k1 As Integer = r.Next(0, 1)
        While i >= 0
            If i1 = i And j1 = j Then
                i1 = r.Next(0, 3)
                j1 = r.Next(0, 3)
            Else
                If k1 = 0 Then
                    a(i1, j1) = 2
                    Exit While
                ElseIf k1 = 1 Then
                    a(i1, j1) = 4
                    Exit While
                End If
            End If
        End While
        updatearray()
    End Sub

    Private Sub addnum()
        Dim i1 As Integer = r.Next(0, 4)
        Dim j1 As Integer = r.Next(0, 4)
        Dim k1 As Integer = r.Next(0, 2)
        While i1 >= 0
            If a(i1, j1) <> 0 Then
                i1 = r.Next(0, 4)
                j1 = r.Next(0, 4)
            Else
                If k1 = 0 Then
                    a(i1, j1) = 2
                    Exit While
                ElseIf k1 = 1 Then
                    a(i1, j1) = 4
                    Exit While
                End If
            End If
        End While
    End Sub
    Private Sub updateold()
        For i = 0 To 3
            For j = 0 To 3
                old(i, j) = a(i, j)
            Next
        Next
    End Sub
    Private Sub checknew()

        temp = 0
        For i = 0 To 3
            For j = 0 To 3
                If a(i, j) = old(i, j) Then
                    temp += 1
                End If
            Next
        Next
    End Sub
    Private Sub checkend()
        Dim clow As Integer = 0
        For i = 0 To 3
            For j = 0 To 3
                If a(i, j) <> 0 Then
                    clow += 1

                End If
            Next
        Next
        If clow = 16 Then
            For i1 = 0 To 3
                For j1 = 0 To 3
                    b(i1, j1) = a(i1, j1)
                Next
            Next
            left1()
            clow = 0
            For i = 0 To 3
                For j = 0 To 3
                    If a(i, j) = b(i, j) Then
                        clow += 1

                    End If
                Next
            Next
            If clow = 16 Then
                right1()
                clow = 0
                For i = 0 To 3
                    For j = 0 To 3
                        If a(i, j) = b(i, j) Then
                            clow += 1

                        End If
                    Next
                Next
                If clow = 16 Then
                    up()
                    clow = 0
                    For i = 0 To 3
                        For j = 0 To 3
                            If a(i, j) = b(i, j) Then
                                clow += 1

                            End If
                        Next
                    Next
                    If clow = 16 Then
                        down()
                        clow = 0
                        For i = 0 To 3
                            For j = 0 To 3
                                If a(i, j) = b(i, j) Then
                                    clow += 1

                                End If
                            Next
                        Next
                        If clow = 16 Then
                            Me.Hide()
                            Form3.Show()


                        Else
                            For i1 = 0 To 3
                                For j1 = 0 To 3
                                    a(i1, j1) = b(i1, j1)
                                Next
                            Next
                            Exit Sub

                        End If

                    Else
                        For i1 = 0 To 3
                            For j1 = 0 To 3
                                a(i1, j1) = b(i1, j1)
                            Next
                        Next
                        Exit Sub

                    End If
                Else
                    For i1 = 0 To 3
                        For j1 = 0 To 3
                            a(i1, j1) = b(i1, j1)
                        Next
                    Next
                    Exit Sub

                End If
            Else
                For i1 = 0 To 3
                    For j1 = 0 To 3
                        a(i1, j1) = b(i1, j1)
                    Next
                Next
                Exit Sub
            End If
        Else
            Exit Sub

        End If


    End Sub
    Private Sub Form1_KeyUp(sender As Object, e As KeyEventArgs) Handles MyBase.KeyUp
        If e.KeyCode = Keys.Up Then
            up()
            checknew()
            If temp = 16 Then

            Else

                addnum()
                score += spot
            End If

            updatearray()
            updateold()
            Label18.Text = score
            checkend()

        End If
        If e.KeyCode = Keys.Down Then
            down()
            checknew()
            If temp = 16 Then

            Else

                addnum()
            End If
            updatearray()
            updateold()
            Label18.Text = score
            checkend()
        End If
        If e.KeyCode = Keys.Right Then
            right1()
            checknew()
            If temp = 16 Then

            Else

                addnum()
            End If
            updatearray()
            updateold()
            Label18.Text = score
            checkend()
        End If
        If e.KeyCode = Keys.Left Then
            left1()
            checknew()
            If temp = 16 Then

            Else
                addnum()
            End If
            updatearray()
            updateold()
            Label18.Text = score
            checkend()
        End If
    End Sub

    Private Sub Label18_Click(sender As Object, e As EventArgs) Handles Label18.Click

    End Sub

    Private Sub Label3_Click(sender As Object, e As EventArgs) Handles Label3.Click

    End Sub
End Class
